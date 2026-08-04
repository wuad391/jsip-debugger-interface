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
bright blue across all panes, and one surface — no boxes, just a
divider line along each seam:

```
▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
                                              ◂ back  ·  step ▸  ·  [space] play  ·  q quit
─────────────────────────────┬──────────────────────────────────────────────────────────────
 CALL STACK 5 calls · 1 live │ HEAP                                2 live · 3 nodes · 2 new
      M.add "b" 2 M.empty    │ ▾ #1 · map ⟨string ⇒ int⟩   ▾ m · map ⟨string ⇒ int⟩
 ▎▾ M.add "a" 1 (M.add "b" 2 │  ┌ #1 ───┐                         ▾┌ m ──────── new ┐
 ▎    M.empty)               │  │"b" → 2│                          │"b" → 2         │
      M.add k (v * 2) acc    │  └───────┘                          └ 0x77127f3ee7e8 ┘
      M.add k (v * 2) acc    │                                      ┌──────┴───────┐
    M.fold (fun k v acc ->   │                                      l              r
      M.add k (v * 2) acc) m │                              ┌── new ┐            ┌┄┄┄┐
      M.empty                │                              │"a" → 1│            ┆ ∅ ┆
─────────────────────────────┤                              └───────┘            └┄┄┄┘
 SOURCE map_fold.ml · 15 line│
         (String)            │
     6                       │
  ▾  7 let () =              │
 ▎   8   let m = M.add "a"   │
 ▎         1 (M.add "b" 2 M  │
 ▎         .empty) in        │
─────────────────────────────┴──────────────────────────────────────────────────────────────
 ● ocaml-debug │ map_fold.dump │ map ⟨string ⇒ int⟩ · replay```

- **Call stack** — every call in the run, indented by depth: the current
  step's live chain renders bright, everything already returned or not yet
  reached is dimmed (click one to jump there). Clicking a live frame
  selects it and the source pane follows, marking the caller's line with
  `▸`. Where a call was written is not repeated here — the source pane
  below already has that line highlighted. Long argument lists wrap, and
  a call's `▾`/`▸` glyph folds its whole range behind a `⋯ n` count
  without touching any other pane.
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
  with a dashed `↗` card (which also terminates payload cycles), and a
  re-observed structure's stub replays the shape its id was defined with. Structures carry
  the latest variable name they were observed under (`m ·`, `tbl ·`;
  `#id` when anonymous), root addresses re-stamp from the registry on
  every redraw, and only unreferenced structures get their own section
  header — name · kind · static type, the one this event walked marked
  in blue. Everything
  folds: the `▾`/`▸` glyph beside a card tucks its subtree away — the
  card stays, and a `⋯ n hidden` note appears beneath it — and the glyph
  on a section
  header hides the whole structure behind that name-and-type summary; a
  folded subtree keeps the structures it references hidden with it, and
  folds survive stepping. Nodes are read straight off the wire: every
  kept field arrives under its own label and a field holding a walked
  block reads `Child`, so the pane names a hashtbl's `data` edge, a
  Core map's `tree` and a user record's fields without a layout of its
  own. Closures and other undecoded blocks print as `⟨0x…⟩`. Each
  structure is drawn like a CS tree diagram:
  node cards (the structure's name in white riding the border's top
  left, the node's meaning underneath), a parent centered above its
  children, siblings sharing a level under a labeled rail, and a dotted
  gray card where a pointer slot is empty — a nil pointer is still a
  slot, so it gets a box like everything else (an empty pointer and the
  integer zero are the same word in memory, so the pane tells them apart
  by label, and a slot it does not recognize reads `l=0` rather than
  guessing). An edge into a card the
  canvas already drew gets a dashed card too, named by what that card
  holds rather than by the wire's id (`↗ "d" → 4`, not `↗ #7`); picking
  either one tints the other's border to match, so a shared subtree
  reads as one object drawn twice. Cards allocated *at this step* carry a green `new` tag in
  the border's top right. Structures lay side by side, up to three to a
  row, packed bottom-left so a collapse frees real space; a structure's
  column is chosen from its expanded footprint, so collapsing one leaves
  the others where they are. Clicking a card jumps the replay to the
  step that allocated it; the wheel scrolls.
- **Transport** — across the top: a bar with one tick per event (click
  to jump) over the controls, right-aligned chips that double as the key legend —
  `◂ back · step ▸ · [space] play · q quit` — every chip clickable. The
  session bar (dump name, structure) sits along the bottom.

## Run it

```sh
dune exec app/bin/main.exe -- -dump-file testing/expected/map_nested.dump
```

`testing/` holds golden dumps of real `-visual-replay` runs, vendored
verbatim from the compiler repo (see `testing/README.md`) — any of them
replays. For **structure sharing**, replay `map_spine_sharing` and step
to the end: a five-node map sits beside the version one more `add`
returned, and the two `↗` cards in the other tree are the subtrees that
`add` did not rebuild. Aim at one with `s` and the card it names, over
in `m`, takes an orange border — the same allocation, drawn twice.

```sh
dune exec app/bin/main.exe -- -dump-file testing/expected/map_spine_sharing.dump
```

`-source-root DIR` says where the dump's relative source paths
live (default: the current directory; the golden dumps' paths resolve
from the repo root).

### Selecting

`Tab` moves focus between the call stack and the heap; the focused
pane's seams turn orange. Inside it, `wasd` aims — the card or row you
are aiming at goes orange while the one you chose stays blue, so both
"where I am" and "where I would land" are on screen at once — and
`Enter` commits, which does exactly what clicking there does: a heap
card jumps the replay to the step that allocated it, a live stack row
selects that frame, a dimmed one jumps to its call. `WASD` skips the
aiming step and commits in one keystroke.

In the heap the cursor walks the tree rather than the picture: `w`/`s`
climb to a card's parent and descend to its first child, `a`/`d` run
along the layer it sits on. A layer is a depth in one tree, not one
parent's children, so `a` from `"j"` reaches its cousin `"b"` two
subtrees away. Empty slots place no card and are skipped; an `↗` card
does place one, and standing on it tints the border of the card it
names without moving you there — you stay in the tree you are reading.

Structures are the outermost layer. From a root, `a`/`d` step to the
structure beside it and `w` to the one before it, and `s` off a leaf
falls through to the structure after — so the whole registry is
reachable without touching the mouse.

The chosen and aimed cards are the only ones that spell out an address,
and it rides the bottom border rather than taking a row. Every card is
spaced as though it were showing one, so picking a card widens that card
into room already set aside for it and nothing else on the canvas moves
— a diagram that reshuffled under every keypress would be unreadable.

The heap pane pans sideways to keep the card you are pointing at in
view; a tree wider than the pane would otherwise keep its right-hand
cards off screen for good.

Beyond the controls row up top, `h`/`l`/`p`/`n` also step, `g`/`G` jump
to the ends, and `PgUp`/`PgDn` scroll the heap.

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
