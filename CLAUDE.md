# CLAUDE.md — jsip-debugger-interface

## What this repo is

The **interface half** of the JSIP visual debugger: a GDB-style terminal UI
that replays a `-visual-replay` dump. Step through an OCaml program's run
and watch the call stack, the source position, and the heap shape of every
tracked data structure evolve.

```
~/jsip_debugger (the compiler fork)          this repo
  ocamlc -visual-replay foo.ml                 bonsai_term TUI
  ./foo  →  vreplay.dump                  →    step-through replay
            (one sexp event per line)
```

`README.md` is the user-facing side — what the panes mean, the keys, a
rendered screenshot. Read it once; this file is about working *in* the repo.

The compiler half lives at `~/jsip_debugger` (GitHub
`ClaraY05/jsip-debugger-compiler`, work branch `vreplay-main`) and has its
own `CLAUDE.md`. **The wire format is a contract between the two repos** —
see "The wire contract" below before touching `lib/types` or `lib/parsing`.

## Build, test, format

**The toolchain is the OxCaml switch `5.2.0+ox`, not stock OCaml.**
`bonsai_term` and the rest of the Jane Street `v0.18~preview` closure only
exist in the OxCaml opam repository and build against the OxCaml compiler
variant. Do not "fix" the project onto vanilla OCaml or swap `bonsai_term`
for something else — the switch is a deliberate choice.

```sh
opam switch create 5.2.0+ox \
  --repos ox=git+https://github.com/oxcaml/opam-repository.git,default
opam install . --deps-only --with-test
```

From there it is ordinary dune:

```sh
dune build                    # compile
dune runtest                  # expect tests
dune runtest --auto-promote   # accept expect diffs — read the diff first
dune fmt                      # format (.ocamlformat: janestreet, margin 77)
dune build @doc               # odoc HTML
dune exec app/bin/main.exe -- -dump-file testing/expected/map_nested.dump
```

Never modify anything under `_build/` — dune regenerates it.

### Two dune gotchas, both already worked around

- **Inside a worktree, always pass `--root .`.** Claude Code's worktrees
  live at `.claude/worktrees/<name>/`, i.e. *nested inside* the main
  checkout, so a bare `dune build` walks up to the outer `dune-project` —
  it announces `Entering directory '/home/.../jsip-debugger-interface'` and
  then fails with `No rule found for alias .claude/worktrees/<name>/default`,
  because the worktree is a gitignored subdirectory of that project, not a
  project. Every dune command in a worktree wants `dune build --root .`,
  `dune runtest --root .`, and so on.
- **`-source-tree-root .` in every test `dune`.** This switch's
  `ppx_expect` resolves dune's `-source-tree-root` against the test file's
  basename, so writing an expect correction crashes. Each test stanza
  overrides it back to the test directory (last flag wins); the comment in
  `lib/parsing/test/dune` is the canonical explanation. Keep the flag when
  adding a test library.

### CI

`.github/workflows/ci.yml` runs build → `dune runtest` → `dune build @fmt`
on every push to `main` and every PR, on the same OxCaml toolchain (the
first run compiles the compiler, ~30 min; later runs hit setup-ocaml's
cache). `.github/workflows/claude.yml` answers `@claude` mentions in issues
and PRs.

Formatting is checked in CI, so run `dune fmt` before pushing.

## Project layout

```
lib/types/     wire-shaped data: calls, locations, snapshots, the call stack
lib/parsing/   dump reader (depth markers + event sexps) and source loader
lib/replay/    the replay model: per-step frames, fresh addresses, captions
app/tui/       the bonsai_term interface
  components/  reusable building blocks: panes, bars, panel, theme, layout
  src/         the app itself: state machine, event loop, wiring
  test/        picture tests over the components and app pieces
app/bin/       the executable (Command_unix, -dump-file / -source-root)
testing/       golden dumps + their programs, vendored from the compiler repo
```

Each `lib/<x>/` has `src/` and `test/`. New code goes in one of those, or in
a new `lib/<new>/{src,test}` — not loose at the top of an existing
directory. UI code goes under `app/tui`: a reusable view builder belongs in
`components/` (the `jsip_tui_components` library), app state and wiring in
`src/` (`jsip_tui`, which re-exports the components so `app/bin` and the
tests have one entry point).

The pipeline, in dependency order:

`Dump_reader.read_until_empty` reads a dump file line by line, tracks the
`{`/`}` depth markers, hands each line's sexp to `Dump_wire.of_string`, and
calls back with a `Call.Info.t` → `Call_stack.create` assembles the
timeline → `Replay.create` walks it once and precomputes every step's view
(live frames, live structures, which addresses are new at that step, the
status caption) → `Jsip_tui.App` renders and drives it. The pane modules
under `app/tui/components/` are pure view builders over `Replay.Step.t`,
themed by `Theme` and framed by `Panel`; `Layout` decides where panes sit
so drawing and mouse hit-testing agree.

## The wire contract

`lib/types/src/snapshot.ml` and `lib/parsing/src/dump_wire.ml` **mirror the
compiler's `vreplay/sexp.mli` field for field**, so the derived
`ppx_sexp_conv` readers here are exact inverses of the compiler's
hand-rolled emitters (the compiler build has no ppx and must not grow one).
When the compiler's schema changes, the change lands here as a type change,
not a parser change — there is no string parsing left on either side.

Rules that keep it working:

- **Extra fields are allowed *only* on the event wrapper** (`Dump_wire.t`,
  via `[@sexp.allow_extra_fields]`), so a newer compiler that adds a
  wrapper field doesn't break an older interface. Every nested type
  rejects extras deliberately — don't loosen them.
- `Snapshot.Ds_type` mirrors the compiler's `Data_structure.t`; a newly
  tracked structure shows up here as a new constructor, and a dump naming
  one this repo doesn't have **fails to parse**. This side lags on purpose
  — the compiler lands a structure first — so expect to add constructors
  before you can read a freshly vendored dump. As of 2026-08-03 `main` has
  `Map`/`Set`/`Queue` while the compiler also emits `Hashtbl` and `User`
  (values of the program's own declared types, which are event roots in
  their own right now).
- After any deliberate format change, re-vendor `testing/` from the
  compiler repo rather than hand-editing fixtures.

`testing/README.md` explains what is vendored and why; the compiler's
`vreplay/sexp.mli` is the schema's prose spec and is worth reading in full
before changing either side.

## `testing/` is data, not source

`testing/` is byte-for-byte compiler output — golden dumps plus the programs
that produced them — copied from the compiler repo's own `testing/`. The
root `dune` marks it `(data_only_dirs testing)` **on purpose**: the dumps'
`loc` line numbers and character ranges must keep matching the sources, so
neither ocamlformat nor the compiler may touch them. Never edit a `.dump` or
a `cases/*.ml` by hand; regenerate in the compiler repo
(`testing/run_tests.sh --promote`) and copy across.

The empty `.dump` files are negative cases (plain functions, partial
application, untracked structures): runs that fire no events.

## Branches, PRs, worktrees

Remote is `origin` = `wuad391/jsip-debugger-interface`; PRs target `main`.
Several people and agents work this repo at once, so:

- Work in a worktree, one per topic. Claude Code's `EnterWorktree` puts them
  under `.claude/worktrees/<name>` on a `worktree-<topic>` branch, which is
  the convention the existing branches follow.
- Run `git worktree list` before creating one; `git worktree prune` clears
  entries marked prunable.
- The git stash stack is **shared across all worktrees**. Never use bare
  `git stash` / `git stash pop` — you can pop someone else's work. Use a WIP
  commit, or `git stash push -m "<unique-tag>"` and `git stash apply <sha>`.

Commit subjects are `feat:`/`fix:`/`refactor:`/`docs:` and describe the
behavior change, not the files touched.

## Code conventions

Match the existing style; don't introduce alternatives without a reason.
`.claude/skills/ocaml-style/` holds the long-form versions of everything
below.

### Documentation

- Every lib needs docs
- Every module needs a comment
- All mli needs `(** doc *)`
- No useless comments (e.g., "adds numbers")
- Show examples
- Say how it fits with other modules
- Doc comments immediately after: `field : type (** doc *)`
- Use `[code]` and `{[blocks]}` in docs
- Use `{!Module.foo}` for links in docs
- `(*_ *)`: ignored by doc tools

### Naming

- Short scope = short name
- Bools: `is_foo` not `check_foo`
- Can raise? End with `_exn`
- Grabs/frees stuff? Start with `with_`
- American English only
- `snake_case` not `camelCase`
- `_`: unused only
- `unsafe`: can segfault. Name it `unchecked` otherwise
- No negative bools (e.g., `dont_foo`)
- Name constants
- Type params: `'a 'b` unless special (`'ok 'err`, `'k 'v` for maps)

### Printing

- `[%string "x is %{x}"]` not `sprintf`
- Always use `sprintf !` for `ppx_custom_printf`
- Always derive `sexp_of`
- `[%message]` > `[%sexp]` for humans

### Testing

- Make readable; use expect tests; tests in a separate dir
- Test-only stuff in `For_testing`
- Test files are named `test_<module>.ml` and live in `lib/<x>/test/`.
- Tests are `let%expect_test "<name>" = ...` with `[%expect {| ... |}]`
  blocks. `let%test` is fine for property-style boolean checks;
  `let%test_unit` for tests without expected output.
- When updating expect output, run `dune runtest --auto-promote` — but
  **read the diff first**. A surprising diff is a real signal.
- Tests that read fixtures declare them:
  `(deps (glob_files_rec ../../../testing/*))`. Prefer a real golden dump
  over an invented one — the point of vendoring them is that the reader is
  tested on exactly what production feeds it.
- TUI tests render into a `notty-community` buffer and expect the drawn
  cells, so a layout change shows up as a readable picture diff.

### Interfaces

- Most modules have `type t`
- Most types are called `t`
- Args: `?optional`, `t`, positional, `~labeled`; label unclear args
- No new infix ops
- No `helpers.ml`
- Avoid functors (use first-class modules)

### Managing namespaces

- Only open if made for opening (`Let_syntax`, `O`, `Composition_infix`)
- `_intf.ml` for shared types
- Make a top-level lib module (see `lib/types/src/jsip_types.ml`) that
  re-exports the lib's modules; add new modules to it and its `.mli`
- Small lib = one module
- Don't alias modules (if must: keep name same)

### Style preferences

- Short match first
- Match > if
- No `else ()`
- No `let...and...` (except monads)
- Type annotations > module paths
- Normal variants > poly variants
- `f();` not `let () = f()`
- Pass `[%here]` when function takes `Source_code_position.t`
- `^/` for paths
- `Time_ns` > `Time_float`

### Avoiding error-prone idioms

- No `| _ ->` when matching on variants
- Write types on ignored stuff (except record fields, labeled args, variant args)
- Use returned values
- No polymorphic compare

### Error handling

- Explicit error types; no `exception` in interfaces
- Raise: `_exn` only; make `ok_exn` visible
- Check human input (`sexp`/`json`); machine formats (`bin_io`) need no validation
- Add context
- For library-internal precondition violations: `raise_s [%message "..." (x : T.t)]`.
- For fallible operations exposed at module boundaries: return `'a Or_error.t`,
  build errors with `Or_error.error_s [%message ...]`.
- Prefer `Or_error.t` over `Result.t` directly.
- A dump is **machine-written but externally supplied**: parse failures come
  back as `Or_error.t` carrying the offending line, not as exceptions
  escaping the reader, and a missing source file renders as a placeholder
  pane rather than crashing the app.

### Opens

```ocaml
open! Core              (* always, for every src/test/bin .ml *)
```

The `!` suppresses unused-open warnings. Don't replace `Core` with
`Stdlib`; don't import individual functions from `Core`. The other opens in
use are `Async` (the app and the TUI event loop) and this repo's own
`Jsip_types` / `Jsip_parsing` / `Jsip_replay` / `Jsip_tui` top-level lib
modules.

### Dune files

Libraries follow a uniform pattern:

```
(library
 (name jsip_<x>)
 (public_name jsip-debugger-interface.jsip_<x>)
 (libraries <deps>)
 (preprocess (pps ppx_jane)))
```

Tests — note the `-source-tree-root .` workaround described above:

```
(library
 (name <x>_test)
 (libraries jsip_<x> expect_test_helpers_core core)
 (inline_tests
  (flags (:standard -source-tree-root .)))
 (preprocess (pps ppx_jane)))
```

`lib/tui/src/dune` additionally preprocesses with `bonsai.ppx_bonsai`.
dune discovers libraries automatically as long as they have a `dune` file;
a new lib also needs its dependencies listed in `dune-project`, which
generates `jsip-debugger-interface.opam` — edit `dune-project`, never the
`.opam`.
