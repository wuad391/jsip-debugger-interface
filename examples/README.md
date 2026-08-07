# examples/ -- a program worth stepping through, with its own dump

`testing/` holds the fixtures the readers are tested against: small,
numerous, and each one aimed at a single case. This directory holds the
opposite -- one program written to be *read*, big enough that the
interface has something to show and small enough to follow.

## order_book

A limit order book with price-time priority, built out of Core the way a
real one is. Five container kinds working on one set of records:

| container | holds |
|---|---|
| `Map` | symbol → book, and price → level |
| `Hash_queue` | the orders resting at one price, oldest first |
| `Hashtbl` | order id → order, so a cancel is O(1) |
| `Hash_set` | the participants who have traded |
| `Fdeque` | the tape of recent fills |

It is the best thing in either repo for looking at the heap pane,
because every container holds the **same** `order` records -- a resting
order is in its price level, in the id index, and named by every fill it
takes part in. The debugger draws each order once and points at it from
the three places it appears, rather than drawing three copies. Sharing
is the whole reason the pane resolves `Id` references instead of
inlining, and this is where you can see it.

`order_book.ml` also documents, in its header comment, the one thing you
have to know when writing a program to be replayed: an event rooted at a
*mutation* needs a named identifier to hang off, so the file binds a
container to a name before mutating it.

## Replaying it

The dump and the profile are here beside the source, so this works from
this repo's root with nothing else installed:

```sh
dune exec app/bin/main.exe -- \
  -dump-file examples/order_book/order_book.dump \
  -perf-file examples/order_book/heat.sexp
```

and the same three flags drive the web interface:

```sh
dune exec app/web/server/serve.exe -- \
  -dump-file examples/order_book/order_book.dump \
  -perf-file examples/order_book/heat.sexp \
  -port 8080
```

No `-source-root` in either: the dump's `loc` strings are relative to
the umbrella repo's root (`examples/order_book/order_book.ml`), and the
same path resolves here, which is why the file sits at exactly this
path and must keep sitting there.

`heat.sexp` is the perf stage's per-function compute profile. It is what
colors the call stack and the flame drawer by sampled compute rather
than by call frequency; drop the flag and the interface falls back to
the trace itself.

## Where it came from, and how to regenerate

Written in the umbrella repo, `ClaraY05/jsip-visual-debugger`, at
`examples/order_book/`; the dump and profile here are the verbatim
output of its pipeline (`canary.sh examples/order_book/order_book.exe`),
captured against compiler pin `1cc4593f65b7816f51e2efc1d18ce276b91cd0bf`.
To refresh them, re-run that pipeline there and copy
`_vreplay/order_book/{order_book.dump,heat.sexp}` back here along with
the source, all three together -- a dump and a source that disagree by
even one line put the source pane on the wrong line.

The root `dune` marks this directory `data_only_dirs` for that reason:
the formatter must not touch `order_book.ml`. Its `dune` file is the
umbrella's, kept so the program can be built and rerun there; nothing in
this repo compiles it.
