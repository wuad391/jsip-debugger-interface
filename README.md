# JSIP Debugger Interface

A GDB-style terminal interface for examining the behavior of OCaml
programs. A jsip_debugger compiler fork, run with `-visual-replay`,
instruments tracked data structures (stdlib `Map`/`Set`/`Queue`/`Hashtbl`) and logs
one event per instrumented call — its location, arguments, the live
registry, and a walked snapshot of the structure's heap shape. This
program replays that log: step through the run and watch the call stack,
the source position, and the allocated data structures evolve.

Pass `-perf-file heat.sexp` (the per-function compute profile the
visual-debugger pipeline's perf stage writes) and the call stack renders
each callee's name in its function's share of the sampled compute — cold
slate through gold to red; a call the profile has no data on keeps its
ordinary color. Without a profile the same ramp falls back to the trace
itself — each function's share of the dump's events — so a replay still
reads hot-to-cold at a glance. The session bar's legend names which of
the two the colors mean (`compute` or `calls`).

Built with [bonsai_term](https://github.com/janestreet/bonsai_term) — the
interface is the design mockup's layout in terminal cells, its warm-gray
palette re-pitched for dark terminals — selection and position in one
bright blue across all panes, and one surface — no boxes, just a
divider line along each seam:

```
▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀ ▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
◂ back · step ▸ · [space] play · ↑↓ node · ⏎ diagram · h fold · z accordion · / filter · f flame · q quit
─────────────────────────────┬──────────────────────────────────────────────────────────────
 CALL STACK 5 calls · 1 live │ HEAP                                2 live · 3 nodes · 2 new
      M.add "b" 2 M.empty    │ ▾ #1  int M.t  1 binding
 ▎▾ M.add "a" 1 (M.add "b" 2 │ └─   "b" → 2
 ▎    M.empty)               │ ▾ m  int M.t  2 bindings  new                 0x73137dfeaa48
      M.add k (v * 2) acc    │ ├─   "b" → 2  new
      M.add k (v * 2) acc    │ └─   "a" → 1  new
  ▾ M.fold (fun k v acc ->   │
      M.add k (v * 2) acc) m │
      M.empty                │
─────────────────────────────┤
 SOURCE map_fold.ml · 15 line│
         (String)            │
     6                       │
  ▾  7 let () =              │
 ▎   8   let m = M.add "a"   │
 ▎         1 (M.add "b" 2 M  ├──────────────────────────────────────────────────────────────
 ▎         .empty) in        │ ▾ FLAME       5 events · width = calls · color = compute
     9   let doubled =       │
     10    M.fold (fun k v   │   ▏M.add            M.add
     11      acc -> M.add k  │   ▏M.add     M.fold
─────────────────────────────┴──────────────────────────────────────────────────────────────
 ● ocaml-debug │ map_fold.dump │ map ⟨string ⇒ int⟩ · replay          heat █████ compute```

(In the drawer each label sits on a filled, colored box running the width
its subtree earned; the gaps are exposed self time.)

- **Call stack** — every call in the run, indented by depth: the current
  step's live chain renders bright, everything already returned or not yet
  reached is dimmed (click one to jump there). Clicking a live frame
  selects it and the source pane follows, marking the caller's line with
  `▸`. Where a call was written is not repeated here — the source pane
  below already has that line highlighted. Every other row wears a faint
  zebra stripe and a blank line separates consecutive calls, so a wall of
  forty reads as rows rather than as a texture; the gaps are nobody's — no
  stripe, no wash, and a click on one lands nowhere. Long argument lists
  wrap, and a call's `▾`/`▸` glyph folds its whole range behind a `⋯ n`
  count without touching any other pane. Exchange-scale dumps repeat
  themselves — thousands of identical leaf calls while a book fills — so
  a run of four or more collapses to one `fn args ⋯ ×N` row whose glyph
  expands it; a run holding the selection or a live frame never
  collapses.

  The call that put a structure into the registry wears its heap name —
  `· m`, `· #826` — so the heap's anonymous `#N`s point at a findable row
  here (committing the heap row was already a jump to this very step).
  The pane centers itself on the selection; the wheel scrolls on top of
  that centering, and any step or aim resets the offset, so the hand
  wins between moves and the centering wins whenever something moves.
- **Source** — syntax-highlighted, the active line washed in the accent
  color, the event's character range underlined; long lines wrap under a
  blank gutter, and top-level definitions fold to their first line plus
  a `⋯ n lines` marker (a fold hiding the active line takes the wash in
  its place). The pane always shows the selected frame's file at its
  line: every file the dump mentions is loaded up front, so on a dump
  spanning twenty modules the source swaps as the blue selection moves
  across the call stack — which is also why stack rows carry no file
  info of their own. A file that is not where the dump said renders a
  placeholder naming where it looked (run from the replayed program's
  root, or pass `-source-root`).
- **Heap** — every live tracked structure as an indented outline, the way
  a file browser shows a directory: one line per thing the structure
  holds. A structure keeps the shape of its most recent walk and only
  leaves the pane when the registry drops it. Each is a top-level row —
  the latest variable name it was observed under (`m`, `tbl`; `#id` when
  anonymous), its type, either what its root record says (`length 2`) or
  how much it holds (`2 bindings`), and its size (`3 nodes · 104 B`) —
  with its contents underneath, a blank line between structures (nobody's:
  unwashed, unclickable, stepped over by the cursor), and the one this
  event walked reads in blue. The sizes are read off the wire's own words,
  8 bytes each — a floor, since an undecoded pointer counts one slot — and
  the pane meta totals the live structures the same way.

  The outline is **logical, not physical**. A walked map is an AVL tree
  and a walked hashtable is an array of AVL trees, but neither is what
  you came to read, so three rules — needing no per-container knowledge
  beyond which labels are a structure's own skeleton — turn a heap shape
  into the contents it stands for: a node with nothing of its own to
  print is plumbing (a bucket array, a wrapper record) and the things it
  points at take its place; an edge whose label is the structure's
  skeleton carries on *through* the container, so what it reaches is a
  sibling rather than a child; and so does an edge landing on a node
  shaped exactly like its source — a cons cell's tail, a map node's
  subtree. So a map lists its bindings, a hashtable its entries and a
  list its elements, all at one depth, while a record's fields still nest
  under it. Empty skeleton slots are not lines: they are most of a tree's
  nodes and none of its content.

  Dumps are deltas: every node's definition appears once under a wire id
  and later occurrences are `Id` references, so the pane resolves them
  against the running node table — a referenced structure nests under the
  row that reaches it (a map added to a queue hangs off the element it
  became), a shared block lists once and later slots point at it with a
  `↗` row beside the ones it shares a level with (which also terminates
  payload cycles), and a re-observed structure's stub replays the shape
  its id was defined with. Values are read straight off the wire: every
  kept field arrives under its own label and a field holding a walked
  block reads `Child`, so the pane names a hashtbl's `data` edge, a Core
  map's `tree` and a user record's fields without a layout of its own; a
  binding whose data is a block of its own reads `"k" →` with the block
  on the line below. Closures and other undecoded blocks print as
  `⟨0x…⟩`, and a row the wire left with nothing at all to say — an empty
  container's one entry, a revisit stub the registry could not resolve —
  reads `null`, spelled out so it cannot be mistaken for a value. A row
  allocated *at this step* carries a green `new` tag. The pane speaks
  four text registers — names bright, types in the calm blue, values in
  the warm orange, sizes faint — plus the fade, which is more than a
  reader carries in from other tools, so a legend sits in the pane's
  bottom-right corner with each word wearing its own color. It hides
  when the pane is too narrow for the whole line.

  A structure the program can no longer name **fades** — guides, glyph,
  name (which loses its bold), type, values and stats, every row of its
  subtree: after `let m = M.add "a" 1 m` both versions are alive and both
  are called `m`, so the older one greys out and its row says `shadowed`
  (`out of scope` for one whose binding has been left behind, like a
  queue built inside a function that has returned). The note is part of
  what the structure says about itself, so `/shadowed` filters the faded
  ones out — and aiming at a faded row still lights it orange, which is
  the one thing you can still do with a structure out of reach. Fading is
  per drawing, not per block, which is how sharing keeps reading
  correctly: after `M.remove` the surviving subtree shows faded among the
  old version's rows and lit as the whole of the new one, because those
  are two things to say about one object. A structure nested under
  another keeps its own verdict, so a live map hanging off a shadowed
  queue cell stays lit — and the diagram pop-out draws whatever verdict
  the popped structure carries, saying why in its meta line.

  Nothing is cropped. A row too wide for the pane wraps onto continuation
  lines, hanging under its own first column so the guides still read down
  the page — a wide record or an inlined float array costs height rather
  than going unread, and there is nothing to pan.

  Anything with something under it folds: the `▾`/`▸` glyph before a row
  tucks its children away behind a `⋯ n` count while the row itself
  stays, and folding a structure's own row collapses the whole structure
  to that one line. A folded subtree keeps the structures it references
  hidden with it, and folds survive stepping. Clicking a row jumps the
  replay to the step that allocated it; clicking the glyph folds instead.

  Stepping lands the pane on the structure the event walked — its row
  brought into view, roughly centered — and from there the wheel scrolls
  freely: only the cursor drags the window with it, so scrolling away to
  read something else is never fought. `.` (or the `. latest` chip) snaps
  back to the walked structure whenever you have wandered.
- **Diagram pop-out** — `Enter` (or the `⏎ diagram` chip) draws the
  structure the cursor is in the way a CS textbook would, over the panes:
  a box per node, rails for the pointers, children spread under their
  parent, empty slots as dotted `∅` boxes. The outline is the everyday
  reading precisely because it hides all that — but the tree is what the
  program actually built, and some questions only its shape answers (why
  one `add` rebuilt three nodes, how deep a bucket chain ran). `Escape`
  puts it away with everything else exactly where you left it; `Enter`
  again takes the jump the row would have taken, to the step that
  allocated it. While it is up it owns the keyboard: `↑↓←→`, `[`/`]` and
  `PgUp`/`PgDn` move around a diagram bigger than the slab — which it
  often is, since this is the format that spreads a tree sideways — and
  nothing steps until you close it. The wheel scrolls it, and the wheel
  with `ctrl` or `alt` held pans it sideways. Not `shift`: the terminal's
  mouse encoding has a shift bit, but notty decodes only the ctrl and
  meta ones, so shift+wheel arrives indistinguishable from a plain
  wheel — `ctrl`/`alt` or `[`/`]` are the sideways gestures there are.

  ```
  ┌────────────────────────────────────────────────────────────────────────────┐
  │ DIAGRAM                              bigger · int M.t · 6 nodes · esc back │
  │                                                                            │
  │                             ┌ bigger  new ┐                                │
  │                             │"f" → 6      │                                │
  │                             └─────────────┘                                │
  │                          ┌─────────┴──────────┐                            │
  │                          l                    r                            │
  │                      ┌───────┐            ┌── new ┐                        │
  │                      │"d" → 4│            │"h" → 8│                        │
  │                      └───────┘            └───────┘                        │
  │                     ┌────┴────┐         ┌─────┴──────┐                     │
  │                     l         r         l            r                     │
  │                 ┌───────┐   ┌┄┄┄┐   ┌── new ┐   ┌────────┐                 │
  │                 │"b" → 2│   ┆ ∅ ┆   │"g" → 7│   │"j" → 10│                 │
  │                 └───────┘   └┄┄┄┘   └───────┘   └────────┘                 │
  │                                                                            │
  └────────────────────────────────────────────────────────────────────────────┘
  ```

  That is the same `bigger` the outline above reads as five bindings and two
  `↗` rows. In here there is only one structure, so there is nothing to
  point at: every node it reaches is drawn, and the count is what was drawn.
- **Flame** — a drawer under the heap, in the right-hand column; `f`
  opens and shuts it. Shut, it keeps only its title row (`▸ FLAME`) and
  gives every other row back to the heap; open (`▾ FLAME`) it is a pane
  like any other and `Tab` reaches it. The whole run's call tree as
  filled boxes, one row per call depth, [the way Brendan Gregg's flame
  graphs are drawn](https://www.brendangregg.com/flamegraphs.html): the
  roots along the **bottom** and their callees stacked above them, so
  the flames rise. Siblings run left to right in **name order**, not by
  size — that is what makes the x-axis alphabetical, so the same program
  always draws the same picture and two captures of it can be read side
  by side.

  A box's **width** is how many calls ran inside it (call *volume*, from
  the trace); its **color** is that function's share of sampled compute
  (from `-perf-file`). Gregg colors boxes at random, from a warm palette,
  purely to separate neighbours; this spends the channel on the profile
  instead, and does it in blue — muted slate, sky, azure, vivid blue,
  bright cyan — so intensity climbs with the share and the hottest box is
  the one your eye lands on. The two channels are different measurements
  on purpose — a wide pale box is a function called too often, a narrow hot
  one is a function that is slow, and those have opposite fixes.
  Identical call paths pool into one box, so "called four hundred times"
  is visible at a glance.

  Children tile their parent exactly, and the gap at a row's right end
  is the parent's own calls — a flame graph's exposed top edge, which is
  where self time lives, so it is drawn as nothing rather than as a box.
  A leaf simply ends. Children too narrow to draw pool behind a `+N`
  rather than vanishing. The path the replay is standing in carries a
  `▏` inside each of its boxes and its deepest box is bold — stepping
  moves the mark, so you can play the run and watch it walk its own
  profile. A box the profile has nothing to say about draws neutral
  gray, off the ramp, so "no data" never reads as "cold". Without
  `-perf-file` there is no compute to show, so the color falls back to
  call volume and the meta line says `color = calls` instead of
  `color = compute`.
- **Transport** — across the top: a bar with one tick per event (click
  to jump), past ticks in the position blue and future ones idle gray,
  each cell brightening within its own hue with the allocation it covers
  — so the run's busy phases read straight off the bar — over the
  controls, right-aligned chips that double as the key legend —
  `◂ back · step ▸ · [space] play · . latest · ↑↓ node · ⏎ diagram ·
  h fold · z accordion · / filter · f flame · q quit` — every chip
  clickable, and the mode chips (play, accordion, diagram, flame) light
  up while theirs is on. The middle of the row swaps while the flame
  drawer holds the keyboard (`↑↓ bar · z zoom · Z reset`), because focus
  rebinds those keys and a legend naming keys that no longer work is
  worse than no legend. The row wants 117 columns; narrower than that
  and the right-hand chips crop. The session bar (dump name, structure)
  sits along the bottom.

## Run it

```sh
dune exec app/bin/main.exe -- -dump-file testing/expected/map_nested.dump
```

`testing/` holds golden dumps of real `-visual-replay` runs, vendored
verbatim from the compiler repo (see `testing/README.md`) — any of them
replays. For **structure sharing**, replay `map_spine_sharing` and step
to the end: a five-binding map sits above the version one more `add`
returned, and the two `↗` rows among that version's bindings are the
subtrees `add` did not rebuild. Aim at one with `↓` and the row it names,
up in `m`, lights in a muted orange — the same allocation, listed twice.

```sh
dune exec app/bin/main.exe -- -dump-file testing/expected/map_spine_sharing.dump
```

For **multi-file source following**, replay `multi_file` and step into
the fold (five steps in): the live chain spans `main.ml` and
`inventory.ml`, and selecting the outer frame (`Tab`, then `s` to it,
`Enter`) swaps the source pane to `main.ml` at the fold's line — the
call stack never repeats file names; the source pane is where a frame's
location lives.

```sh
dune exec app/bin/main.exe -- -dump-file testing/expected/multi_file.dump
```

`-source-root DIR` says where the dump's relative source paths
live (default: the current directory; the golden dumps' paths resolve
from the repo root).

### Selecting

`Tab` moves focus between the call stack, the heap, and — while it is
open — the flame drawer; the focused pane's seams turn orange. Inside
it, `↑`/`↓` (or `wasd`) aim — the row you are aiming at goes orange
while the one you chose stays blue, so both "where I am" and "where I
would land" are on screen at once.

What `Enter` then does depends on which pane has the keyboard. In the
**heap** it pops the diagram out (`Escape` back, `Enter` again to go
through to the allocation step) — the outline is the everyday reading and
the tree is one keystroke away. In the **call stack** it commits: a live
row selects that frame, a dimmed one jumps to its call. In the **flame
drawer** it jumps to the first call that merged into the aimed bar.
Clicking a heap row commits it either way, and `WASD` aims and commits
in one keystroke in every pane.

In the heap the cursor walks the outline the way a file tree walks.
`↑`/`↓` (`w`/`s`) step to the line above and below, crossing from one
structure into the next without being asked to, so one key runs the whole
pane top to bottom — including a collapsed structure, which is nothing
but its own line, so `h` there opens it again. `a`/`d` climb to the line
this one hangs under and drop into the first line under it; a folded row
has nothing under it to drop into, which is the point — fold what you are
done with and `↓` steps past it.

Standing on an `↗` row lights the row it names in a muted wash of the
same color, without moving you there — a shared subtree is the one thing
on the pane you go looking for from across it. The row you are actually
on keeps the full wash and the address, so the pair never reads as two
cursors.

The chosen and aimed rows are the only ones that spell out an address,
and it rides the right margin of the row's last line — placed once the
wrapping is settled, so a row never reflows around its own address.

`h` collapses whatever the focused pane is pointing at — in the heap the
row the cursor is on, or the whole structure from its top line; in the
call stack the aimed call's range. Pressing it again expands. An outline
is one column, so a fold can only ever take lines away: everything below
closes up by exactly what was hidden.

### Hundreds of structures

A real program's registry is not three maps — an exchange run carries a
thousand live structures — so the heap pane has two ways to cut it down,
both announced on its meta line.

`z` toggles **accordion** mode: every structure collapses except the one
the keyboard is in, so the pane becomes a list of one-line
`name · type · what it holds` summaries plus wherever you are standing.
The fold set is recomputed from the cursor, which means walking `↑`/`↓`
across the registry opens each structure as you arrive and closes it
behind you. Your own structure folds are the accordion's to override
while the mode is on — row-level folds keep working — and they come
back untouched when it goes off.

`/` opens a **filter** prompt: while it is open every key spells the
filter (so `wasd`, `space` and `q` type instead of acting), the pane
narrows live as you type, and only structures matching — name, kind or
type, case-insensitive, so `/order`, `/hashtbl` and `/string` all work —
stay on it. `Enter` keeps the filter,
`Escape` drops it (also from outside the prompt), backspacing past the
last character backs out of the prompt itself, and the meta line
owns up to the cut: `/order · 42 of 1223 live`. A fresh `/` always
starts empty.

`o` flips the outline's **order**. The default is registry order —
creation order, oldest structure first. With `o` on, the top level sorts
by ascending address instead, so memory locality reads as adjacency:
structures allocated near each other sit next to each other. Nested
references keep nesting inside their referrers, folds and the selection
survive the flip, and the meta line owns up with `by address`.

`1` and `2` — or a click on a pane's title row — collapse the **call
stack** and the **source pane** to exactly that title (`▸` and the
counts), handing the freed height to the other left pane; the same key or
click reopens them. Collapsing the stack while it has the keyboard hands
focus to the heap.

### The flame drawer

`f` opens and shuts the drawer under the heap; clicking its title row
does the same, which is the only way in with the mouse while it is shut.
Opening it takes rows from the heap and gives them straight back on
close — nothing else on screen moves. It stays open while you step and
play, which is the point: press `space` and watch the `▏` mark walk the
run's own profile with the heap still beside it.

`Tab` reaches it once it is open (and skips it while it is shut, since a
title row has nothing to aim at). The keys are **spatial**, because the
flames rise: `w`/`↑` climbs *up* the picture, which is deeper into the
run — into the widest callee, which under name ordering is not the
leftmost one — and `s`/`↓` comes back down toward the root. `a`/`d` run
along the callees one caller has, and `Enter`, or a click, jumps the
replay to the first call that merged into the box you are on. (The heap
pane's `w` climbs to a *parent* for the same reason: its tree is drawn
root-first, this one root-last. And `Enter` there pops the diagram out
instead — the jump lives on the bars, the diagram on the outline.)

`z` means zoom here rather than accordion: the box under the cursor is
rescaled to the full width, so children pooled behind a `+N` widen back
into view. `Z` resets it, and the meta line names what you are zoomed to
(`⌖ Map.add`). `PgUp`/`PgDn` and the wheel walk depth when the tree is
taller than the drawer; a tree shorter than it sits on the bottom row
with the spare space above the flames.

Pass `-perf-file heat.sexp` — the profile the pipeline's perf stage
writes — to color the bars by sampled compute. Without it the bars still
draw, colored by call volume from the trace alone, and the meta line
says `color = calls`; a box the *profile* had nothing to say about draws
neutral gray, off the ramp, so "no data" never reads as "cold".

Beyond the controls row up top, `l`/`n` and `p` also step, `g`/`G` jump
to the ends, and `PgUp`/`PgDn` scroll the heap (they walk the drawer's
depth while it holds the keyboard). `Escape` also clears a committed
filter, and — like every other key but its own few — it is the diagram
pop-out's while that is up.

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
