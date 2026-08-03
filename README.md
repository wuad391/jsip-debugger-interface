# JSIP Debugger Interface

A GDB-style terminal interface for examining the behavior of OCaml
programs. A jsip_debugger compiler fork, run with `-visual-replay`,
instruments tracked data structures (stdlib `Map`/`Set`/`Queue`/`Hashtbl`) and logs
one event per instrumented call — its location, arguments, the live
registry, and a walked snapshot of the structure's heap shape. This
program replays that log: step through the run and watch the call stack,
the source position, and the allocated data structures evolve.

Built with [bonsai_term](https://github.com/janestreet/bonsai_term) — the
interface is the design mockup's layout in terminal cells, its warm-gray
palette re-pitched for dark terminals — selection and position in one
bright blue across all panes, and no boxes — single dividers split the
screen into panes:

```
 █████████████████ █████████████████ █████████████████ █████████████████ █████████████████
                                              ◂ back  ·  step ▸  ·  [space] play  ·  q quit


 CALL STACK                 5 calls · 1 live │ HEAP                2 live · 8 nodes · 4 new
      M.add "a" 1         map_versions.ml:10 │ ▾ #7 · map ⟨string ⇒ int⟩
        M.empty                              │            ▾┌ #7 ────────────┐
    M.add "b" 2 (M.add    map_versions.ml:10 │             │ "b" ↦ 2        │
      "a" 1 M.empty)                         │             │ 0x7b7f1dc87708 │
    M.add "c" 3 m1        map_versions.ml:11 │             └────────────────┘
    M.add "d" 4 m2        map_versions.ml:12 │          ┌──────────┴──────────┐
 ▎  M.add "e" 5 m         map_versions.ml:17 │          l                     r
                                             │  ┌────────────────┐   ▾┌────────────────┐
                                             │  │ "a" ↦ 1        │    │ "c" ↦ 3        │
                                             │  │ 0x7b7f0dbe2080 │    │ 0x7b7f0dbe20e0 │
─────────────────────────────────────────────┤  └────────────────┘    └────────────────┘
 SOURCE           map_versions.ml · 17 lines │                          ┌─────┴──────┐
           M.empty) in                       │                          l            r
    11   let m2 = M.add "c" 3 m1 in          │                          ∅    ┌─────────────
    12   M.add "d" 4 m2                      │                               │ "d" ↦ 4
    13                                       │                               │ 0x7b7f0dbe21
  ▾ 14 let () =                              │                               └─────────────
    15   let m = grow () in                  │
    16   Gc.full_major ();                   │ ▾ #11 · map ⟨string ⇒ int⟩
 ▎  17   ignore (M.add "e" 5 m)              │ ▾┌ #11 ────── new ┐
 ● ocaml-debug │ map_versions.dump │ map ⟨string ⇒ int⟩ · replay
```

- **Call stack** — every call in the run, indented by depth: the current
  step's live chain renders bright, everything already returned or not yet
  reached is dimmed (click one to jump there). Clicking a live frame
  selects it and the source pane follows, marking the caller's line with
  `▸`. Long argument lists wrap, and a call's `▾`/`▸` glyph folds its
  whole range behind a `⋯ n` count without touching any other pane.
- **Source** — syntax-highlighted, the active line washed in the accent
  color, the event's character range underlined; long lines wrap under a
  blank gutter, and top-level definitions fold to their first line plus
  a `⋯ n lines` marker (a fold hiding the active line takes the wash in
  its place).
- **Heap** — every live tracked structure: one keeps the shape of its
  most recent walk and only leaves the pane when the registry drops it.
  Dumps are deltas: every node's definition appears once under a wire id
  and later occurrences are `Id` references, so the pane resolves them
  against the running node table — a referenced structure's whole tree
  links in at the reference site (a map added to a queue hangs off the
  queue's cell), a shared block draws once and later slots point at it
  with `↗ #n` (which also terminates payload cycles), and a re-observed
  structure's stub replays the shape its id was defined with. Structures carry
  the latest variable name they were observed under (`m ·`, `tbl ·`;
  `#id` when anonymous), root addresses re-stamp from the registry on
  every redraw, and only unreferenced structures get their own section
  header — name · kind · static type, the one this event walked marked
  in blue, its root card washed in the highlight background. Everything
  folds: the `▾`/`▸` glyph beside a card tucks its subtree away — the
  card stays, and a `⋯ n hidden` note appears beneath it — and the glyph
  on a section
  header hides the whole structure behind that name-and-type summary; a
  folded subtree keeps the structures it references hidden with it, and
  folds survive stepping. Nodes follow the
  emitter's layered layout contract — interior skeleton vs user payload,
  hashtbl's record → bucket array → chains included; closures and other
  undecoded blocks print as `⟨0x…⟩`. Each structure is drawn like a CS
  tree diagram:
  blue-outlined node cards (the structure's name in white riding the
  border's top left, the node's meaning over its full address), a
  parent centered above its children, siblings sharing a level under a
  labeled rail, `∅` where an interior slot is empty. Cards allocated *at
  this step* carry a green `new` tag in the border's top right. Clicking
  a card jumps the replay to the step that allocated it; the wheel
  scrolls.
- **Transport** — across the top: a tall bar with one tick per event
  (click to jump) over the controls, right-aligned chips that double as the key legend —
  `◂ back · step ▸ · [space] play · q quit` — every chip clickable. The
  session bar (dump name, structure) sits along the bottom.

## Run it

```sh
dune exec app/bin/main.exe -- -dump-file testing/expected/map_nested.dump
```

`testing/` holds golden dumps of real `-visual-replay` runs, vendored
verbatim from the compiler repo (see `testing/README.md`) — any of them
replays. `-source-root DIR` says where the dump's relative source paths
live (default: the current directory; the golden dumps' paths resolve
from the repo root). The controls row up top is the key legend; beyond
it, `h`/`l`/`p`/`n` also step, `g`/`G` jump to the ends, and
`PgUp`/`PgDn` scroll the heap.

## Toolchain

This project builds on the [OxCaml](https://oxcaml.org) switch —
`bonsai_term` and the rest of the Jane Street `v0.18~preview` closure come
from the OxCaml opam repository:

```sh
opam switch create 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default
opam install . --deps-only --with-test
```

Standard `dune` from there:

```sh
dune build                 # compile
dune runtest               # expect tests
dune fmt                   # format (janestreet profile)
```

## Layout

```
lib/types/     wire-shaped data: calls, locations, snapshots, the call stack
lib/parsing/   dump reader (depth markers + event sexps) and source loader
lib/replay/    the replay model: per-step frames, fresh addresses, captions
lib/tui/       the bonsai_term interface: panes, theme, layout, app
app/bin/       the executable
testing/       golden dumps + their programs, vendored from the compiler repo
```

Each `lib/<x>/` has `src/` and `test/`; tests are expect tests
(`dune runtest --auto-promote` to accept output changes — read the diff).

## GitHub Actions

- **`.github/workflows/ci.yml`** — builds, tests, and checks formatting on
  every push to `main` and every PR, on the OxCaml toolchain (the first
  run builds the compiler; later runs hit setup-ocaml's cache).
- **`.github/workflows/claude.yml`** — runs the
  [Claude Code Action](https://github.com/anthropics/claude-code-action)
  when someone writes `@claude` in an issue or PR. Needs an
  `ANTHROPIC_API_KEY` secret.
