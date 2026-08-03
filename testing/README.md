# testing/ -- real dumps from the compiler, vendored as fixtures

`cases/` and `expected/` are byte-for-byte copies of the compiler
repo's `testing/` golden files: each `cases/<name>.ml` was compiled
there with `-visual-replay` and run, and `expected/<name>.dump` is the
verbatim output of that run -- exactly what this repo's reader is fed
in production. The parser, replay, and interface tests all run on
them, and the README's demo command replays one.

They are regenerated in the compiler repo (`testing/run_tests.sh
--promote`) and copied here; don't edit them by hand. Because the
dumps were produced from the compiler repo's root, their `loc` strings
are relative to it (`testing/cases/<name>.ml`) -- which resolves here
too, so `-source-root .` from this repo's root finds the sources.

The empty `.dump` files are negative cases (plain functions, partial
application, untracked structures): a run that fires no events.

One exception to "copied from the compiler repo": `map_spine_sharing`
was written here, as a legible demo of structure sharing. Its dump is
still real compiler output -- the `feat/structure-sharing` tree's
`ocamlc` compiled it with `-visual-replay` from this repo's root, and
the compiler's own `check_dump` validated the result -- but the case
does not (yet) live in the compiler's `testing/cases/`, so
`run_tests.sh` there does not cover it. Worth promoting upstream.
