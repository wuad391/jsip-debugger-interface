# JSIP Debugger Interface

A GDB-style terminal interface for examining the behavior of OCaml
programs. A jsip_debugger compiler fork, run with `-visual-replay`,
instruments tracked data structures (stdlib `Map`/`Set`/`Queue`) and logs
one event per instrumented call — its location, arguments, the live
registry, and a walked snapshot of the structure's heap shape. This
program replays that log: step through the run and watch the call stack,
the source position, and the allocated data structures evolve.

Built with [bonsai_term](https://github.com/janestreet/bonsai_term) — the
interface is the design mockup's layout in terminal cells:

```
 ● ocaml-debug │ maps.dump │ map · replay                      PHASE DESCEND │ STEP 4/7
┌ CALL STACK ────────────────────── 3 live ┐┌ HEAP ───────────── map · 1 nodes · 1 new ┐
│  M.add "gamma" 3 t             maps.ml:6 ││ live  1↦0x3a0                            │
│ ▎  M.add "gamma" 3 OMITTED     maps.ml:6 ││                                          │
│      M.add "gamma" 3 OMITTED   maps.ml:6 ││ ● 0x4c0  v="gamma"  d=3  h=1  new        │
└───────────────────────────────────────────┘│ ├─l→ ∅                                   │
┌ SOURCE ─────────────── maps.ml · 8 lines ┐│ └─r→ ∅                                   │
│    5 let t = M.of_list [ "beta", 2; ...  ││                                          │
│ ▎  6 let t' = M.add "gamma" 3 t          ││                                          │
│    7 let t'' = M.remove "beta" t'        ││                                          │
└───────────────────────────────────────────┘└──────────────────────────────────────────┘
────────────────────────────────────────────────────────────────────────────────────────
 ━━━━━━━━━━ ━━━━━━━━━━ ━━━━━━━━━━ ━━━━━━━━━━ ━━━━━━━━━━ ━━━━━━━━━━ ━━━━━━━━━━
 ◂ back  step ▸  ⏵ play  │ ▎ M.add "gamma" 3 OMITTED — maps.ml:6    ◂ ▸ step · q quit
```

- **Call stack** — the frames live at this step, nested by depth; `↑`/`↓`
  (or a click) selects a frame and the source pane follows it, marking the
  caller's line with `▸`.
- **Source** — syntax-highlighted, the active line washed in the accent
  color, the event's character range underlined.
- **Heap** — the walked structure: value fields inline, `l`/`r` pointer
  slots as edges, `∅` for empties, addresses shared with earlier versions
  shown plainly and nodes allocated *at this step* marked `new`. The
  `live` strip is the event's registry. Clicking a node jumps the replay
  to the step that allocated it; the wheel scrolls.
- **Timeline** — one tick per event; click to jump, `space` to play.

## Run it

```sh
dune exec app/bin/main.exe -- -dump-file demo/maps.dump
```

`-source-root DIR` says where the dump's relative source paths live
(default: the dump's directory). Keys: `◂`/`▸` (also `h`/`l`, `p`/`n`)
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
demo/          a hand-written dump + matching source to try the interface on
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
