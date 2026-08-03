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
bright blue across all panes, the gold reserved for the heap's cards:

```
 ━━━━━━━━━━━━ ━━━━━━━━━━━━ ━━━━━━━━━━━━ ━━━━━━━━━━━━ ━━━━━━━━━━━━
                                       ◂ back  ·  step ▸  ·  [space] play  ·  q quit
────────────────────────────────────────────────────────────────────────────────────────────
┌ CALL STACK ────────────── 5 calls · 1 live ┐┌ HEAP ───────────── set · 4 nodes · 4 new ┐
│  S.of_list [1; 2; 3]          set_ops.ml:6 ││            ┌────────────────┐            │
│  S.of_list [3; 4]             set_ops.ml:7 ││            │ 3  new         │            │
│ ▎S.union a b                  set_ops.ml:8 ││            │ 0x714aae9ea098 │            │
│  S.inter a b                  set_ops.ml:9 ││            └────────────────┘            │
│  S.diff a b                  set_ops.ml:10 ││           ┌─────────┴──────────┐         │
│                                            ││           l                    r         │
│                                            ││  ┌────────────────┐   ┌────────────────┐ │
│                                            ││  │ 2  new         │   │ 4  new         │ │
│                                            ││  │ 0x714aae9ea0c0 │   │ 0x714aae9ea1a8 │ │
│                                            ││  └────────────────┘   └────────────────┘ │
└────────────────────────────────────────────┘│     ┌─────┴─────┐                        │
┌ SOURCE ───────────── set_ops.ml · 10 lines ┐│     l           r                        │
│    5 let () =                              ││ ┌────────────────┐   ∅                   │
│    6   let a = S.of_list [ 1; 2; 3 ] in    ││ │ 1  new         │                       │
│    7   let b = S.of_list [ 3; 4 ] in       ││ │ 0x714aae9ea0e8 │                       │
│ ▎  8   ignore (S.union a b);               ││ └────────────────┘                       │
│    9   ignore (S.inter a b);               ││                                          │
└────────────────────────────────────────────┘└──────────────────────────────────────────┘
 ● ocaml-debug │ set_ops.dump │ set · replay
```

- **Call stack** — every call in the run, indented by depth: the current
  step's live chain renders bright, everything already returned or not yet
  reached is dimmed (click one to jump there). Clicking a live frame
  selects it and the source pane follows, marking the caller's line with
  `▸`. Long argument lists wrap.
- **Source** — syntax-highlighted, the active line washed in the accent
  color, the event's character range underlined; long lines wrap under a
  blank gutter.
- **Heap** — every live tracked structure: one keeps the shape of its
  most recent walk and only leaves the pane when the registry drops it.
  A payload field referencing another live structure (an `Id` into the
  registry) links that structure's whole tree in at the reference site —
  so a map added to a queue hangs off the queue's cell. Structures carry
  the latest variable name they were observed under (`m ·`, `tbl ·`;
  `#id` when anonymous), root addresses re-stamp from the registry on
  every redraw, and only unreferenced structures get their own section
  header (the one this event walked marked in blue). Nodes follow the
  emitter's layered layout contract — interior skeleton vs user payload,
  hashtbl's record → bucket array → chains included; closures and other
  undecoded blocks print as `⟨0x…⟩`. Each structure is drawn like a CS
  tree diagram:
  gold-outlined node cards (the node's meaning over its full address), a
  parent centered above its children, siblings sharing a level under a
  labeled rail, `∅` where an interior slot is empty. Cards allocated *at
  this step* get the brighter fresh border and a `new` chip. Clicking a
  card jumps the replay to the step that allocated it; the wheel scrolls.
- **Transport** — across the top: one tick per event (click to jump)
  over the controls, right-aligned chips that double as the key legend —
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
