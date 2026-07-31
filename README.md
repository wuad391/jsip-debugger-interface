# JSIP Debugger Interface

A GDB-style terminal interface for examining the behavior of OCaml
programs. A jsip_debugger compiler fork, run with `-visual-replay`,
instruments tracked data structures (stdlib `Map`/`Set`/`Queue`) and logs
one event per instrumented call — its location, arguments, the live
registry, and a walked snapshot of the structure's heap shape. This
program replays that log: step through the run and watch the call stack,
the source position, and the allocated data structures evolve.

Built with [bonsai_term](https://github.com/janestreet/bonsai_term) — the
interface is the design mockup's layout in terminal cells, its warm-gray
palette re-pitched for dark terminals:

```
 ● ocaml-debug │ map_fold.dump │ map · replay                                 STEP 5/5
┌ CALL STACK ───────── 5 calls · 1 live ┐┌ HEAP ─────────────────────── map · 2 nodes ┐
│    M.add "b" 2 M.empty  map_fold.ml:8 ││ ┌─────────┐                                │
│  M.add "a" 1 (M.add     map_fold.ml:8 ││ │ "a" ↦ 2 │                                │
│    "b" 2 M.empty)                     ││ │ 0x…6a58 │                                │
│    M.add k (v * 2)     map_fold.ml:12 ││ └─────────┘                                │
│      acc                              ││ ├─l→ ∅                                     │
│    M.add k (v * 2)     map_fold.ml:12 ││ └─r→ ┌─────────┐                           │
│      acc                              ││      │ "b" ↦ 4 │                           │
│ ▎M.fold (fun k v acc   map_fold.ml:10 ││      │ 0x…6a88 │                           │
│ ▎  -> M.add k (v * 2) acc) m M.empty  ││      └─────────┘                           │
└───────────────────────────────────────┘│                                            │
┌ SOURCE ─────── map_fold.ml · 15 lines ┐│                                            │
│    8   let m = M.add "a" 1 (M.add     ││                                            │
│          "b" 2 M.empty) in            ││                                            │
│    9   let doubled =                  ││                                            │
│ ▎ 10     M.fold                       ││                                            │
│   11       (fun k v acc ->            ││                                            │
│   12         M.add k (v * 2) acc)     ││                                            │
└───────────────────────────────────────┘└────────────────────────────────────────────┘
──────────────────────────────────────────────────────────────────────────────────────
 ━━━━━━━━━━━━━ ━━━━━━━━━━━━━ ━━━━━━━━━━━━━ ━━━━━━━━━━━━━ ━━━━━━━━━━━━━
 ◂ back  step ▸  ⏵ play        ◂ ▸ step · space play · ↑ ↓ frame · click jumps · q quit
```

- **Call stack** — every call in the run, indented by depth: the current
  step's live chain renders bright, everything already returned or not yet
  reached is dimmed (click one to jump there). `↑`/`↓` (or a click) selects
  a live frame and the source pane follows it, marking the caller's line
  with `▸`. Long argument lists wrap.
- **Source** — syntax-highlighted, the active line washed in the accent
  color, the event's character range underlined; long lines wrap under a
  blank gutter.
- **Heap** — the walked structure as node cards: each box carries the
  node's meaning (`"a" ↦ 2` for a map binding, `length n` for a queue
  root, the element for sets and cells) over the tail of its address;
  pointer slots hang off as labeled edges, `∅` where an interior slot is
  empty, and cards allocated *at this step* get the fresh border and a
  `new` chip. Clicking a card jumps the replay to the step that allocated
  it; the wheel scrolls.
- **Timeline** — one tick per event; click to jump, `space` to play.

## Run it

```sh
dune exec app/bin/main.exe -- -dump-file testing/expected/map_nested.dump
```

`testing/` holds golden dumps of real `-visual-replay` runs, vendored
verbatim from the compiler repo (see `testing/README.md`) — any of them
replays. `-source-root DIR` says where the dump's relative source paths
live (default: the current directory; the golden dumps' paths resolve
from the repo root). Keys: `◂`/`▸` (also `h`/`l`, `p`/`n`)
step · `space` play/pause · `↑`/`↓` frame · `g`/`G` ends · `PgUp`/`PgDn`
scroll heap · `q` quit.

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
