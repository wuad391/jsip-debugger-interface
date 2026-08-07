# Why instrumented programs are slow, and the registry delta that fixes it

Measured 2026-08-07 on the exchange `debug_scenario` (1513 events,
compiler `1cc4593f65`, exchange tree `30baf45`, bytecode `-custom`
build via `canary.sh`'s toolchain), on a loaded 4-core box — user CPU
time, best of 3, is the comparable number throughout.

| build | user time | dump size | TUI load (parse→replay) |
| --- | --- | --- | --- |
| uninstrumented | 0.016 s | — | — |
| instrumented, today | 4.25 s | 22.2 MB | 1.27 s |
| instrumented, registry delta | **2.00 s** | **2.26 MB** | **0.12 s** |

The registry-delta prototype alone halves the runtime overhead, shrinks
the dump 9.8×, and loads in the interface 11× faster. Nothing else
about the wire changed; the folded deltas reconstruct the full registry
**exactly** (verified per event, 1513/1513, by emitting both forms from
one process and comparing).

## Where the bytes go

Field breakdown of the 22.2 MB dump (analyzer script in the appendix):

| field | bytes | share |
| --- | --- | --- |
| `registry` | 20,058,959 | **90.3%** |
| `snapshot` | 1,254,852 | 5.6% |
| `scope` | 285,258 | 1.3% |
| everything else | ~620 k | 2.8% |

The registry echo — every live tracked structure's `(id addr name)`
triple, re-stated on **every** event — is 90% of the dump. It grows
linearly over the run (1 → 997 entries here, mean 488/event; 738,347
triples written for ~1,200 distinct structures). The 32.9 MB / 1827-
event variant of the same scenario measures 92.4%. Snapshots are NOT
the problem: spine sharing already keeps them at a 251-byte median.

## Where the time goes

Phase timers compiled into `vreplay.ml`/`vreplay_registry.ml` (their
`Sys.time` calls add ~1.5 s of overhead, spread roughly evenly — read
proportions, not absolutes; GC time lands in the phase that allocates):

| phase | time | what it is |
| --- | --- | --- |
| render | 2.42 s | sexp tree → string (∝ bytes, so ~92% registry) |
| sexp_build | 1.11 s | **1.05 s of it is the registry triples** |
| live_known | 0.87 s | registry scan → echo triples array |
| live_membr | 0.64 s | member-store weak-slot scan → walk table |
| traverse | 0.37 s | the C walk, echo alloc, strdup, qsort |
| register | 0.36 s | `find_entry`/`find_member_id` linear scans |
| write | 0.08 s | `write(2)` — the syscalls were never the issue |

Roughly **70% of all overhead is producing the registry echo**
(build + its share of render + live_known + the C-side echo). The
`perf` view agrees: 68.5% bytecode interpreter, 11% GC (allocation
churn from the same serialization), ~2% qsort, ~2% printf.

## The fix: dump the registry as a delta

The registry is the only wire component re-stating unchanged facts.
Addresses barely move: over 1513 events this workload saw **1233
upserts total (~0.8/event), 32 drops, 209 address moves** — 0.03% of
the 738 k id-observations — because tracked roots settle into the major
heap, minor GCs (2441 of them) don't touch them, and nothing compacted.
Worst case (a compaction moving everything) costs one full re-emit for
that event, i.e. exactly today's cost, once.

Proposed wire shape, replacing `(registry ...)` on the event wrapper:

```
(registry_delta ((upserts ((3 0x7f.. m) (9 0x7f..))) (drops (4 7))))
```

- `upserts`: entries new since the last event OR whose address or name
  changed, in the exact `sexp_of_registry` entry shape (two-atom
  anonymous form included). A rename is an upsert; ids never recycle,
  so upsert/drop order is immaterial.
- `drops`: ids whose entry the GC retired — explicit now, where the
  full echo implied it by absence.
- Address coherence is unchanged: the compare runs on the triples the
  C walker captured during its no-allocation walk, so an upsert's
  address is the walk-time address, same as today.

Runtime side (compiler repo), the entire prototype — a shadow table in
`vreplay.ml`, compared after `traverse` returns the echo:

```ocaml
let shadow : (int, nativeint * string) Hashtbl.t = Hashtbl.create 64

let registry_delta registry =
  let upserts = ref [] in
  Array.iter
    (fun ((id, addr, name) as trip) ->
      match Hashtbl.find_opt shadow id with
      | Some (a, n) when a = addr && String.equal n name -> ()
      | _ ->
        Hashtbl.replace shadow id (addr, name);
        upserts := trip :: !upserts)
    registry;
  let drops =
    if Hashtbl.length shadow = Array.length registry then []
    else begin
      (* rare: something was collected -- sweep once *)
      let seen = Hashtbl.create (Array.length registry) in
      Array.iter (fun (id, _, _) -> Hashtbl.replace seen id ()) registry;
      let dead = ref [] in
      Hashtbl.iter
        (fun id _ ->
          if not (Hashtbl.mem seen id) then dead := id :: !dead)
        shadow;
      List.iter (Hashtbl.remove shadow) !dead;
      !dead
    end
  in
  ...
```

(The size-mismatch trick works because after the upsert pass the shadow
is a superset of the live registry; they differ exactly when something
died.)

## Migration between the repos

Verified on today's `main`: an event carrying **both** `registry` and
`registry_delta` parses fine — `Dump_wire.t` has
`[@sexp.allow_extra_fields]` on the wrapper for exactly this — and a
delta-only dump fails as a clean `Or_error` naming the missing field.
So the landing order is:

1. **Interface first** (contrary to the ds_type habit, because here the
   interface must not lag): `Dump_wire.t` makes `registry` optional and
   adds optional `registry_delta`; the reader folds deltas so exactly
   one of the two yields each event's registry view. Old dumps keep
   parsing byte-for-byte.
2. **Compiler**: emit `registry_delta`, stop emitting `registry`
   (optionally emit both for one transition PR — old interfaces keep
   working on new dumps during the overlap).
3. Re-vendor `testing/` once the compiler side lands.

Interface-side note: don't materialize a full registry array per step.
`Replay.create` spends 0.49 s of the 1.27 s load on the current dump
building per-step views off those arrays; fold the deltas into a
persistent `Int.Map` per step instead (shared spines, like
`Replay.Structure` already does for walks) and that cost stays gone
even for long dumps.

## The rest of the ranked list

Post-delta, ~2.0 s of overhead remains (~1.3 ms/event). In order:

1. **The per-event weak scans, ~1.5 s of the timed run** — `live_known`
   (whole registry, every event), `live_members` (every member slot:
   493 k Weak.get round-trips ≈ 1 µs each in bytecode), `find_entry` /
   `find_member_id` (linear, and most events' roots are fresh, so the
   miss scans everything). No wire change needed. The honest fix is a
   C helper in the vreplay stubs that walks the weak arrays and fills
   the walk's tables directly — per-slot cost drops from ~1 µs of
   interpreter dispatch to ~10 ns. A GC-generation cache (reuse last
   event's tables when no GC ran in between) sounds cheaper but won't
   hit: this workload averages 1.6 minor collections per event.
2. **Serialization polish, a few hundred ms** — `Sexp.hex` calls
   `Printf.sprintf "0x%nx"` per address (glibc printf shows in perf);
   hand-roll it. Reuse one `Buffer` across events. Optionally emit the
   `{`/`}` markers with the event line in one write (syscalls measured
   cheap, so this is cosmetic).
3. **Native compilation of instrumented programs** — the profile is
   68.5% bytecode interpreter; `vreplay.cmxa` and the asmlink path
   already exist in the fork, but `canary.sh`'s shim pins
   `native_compiler: false` because the fork tree builds no `ocamlopt`.
   Building it and teaching the shim native would cut every OCaml-side
   constant ~5×, multiplicative with all of the above.
4. **Mutable re-walk deltas** — Hashtbl/Queue re-walk in full each
   event where immutables collapse to revisit stubs and `Id` refs. Not
   the bottleneck in this dump (snapshots are 5.6% of bytes) but the
   same delta idea applies eventually: compare against the last walk
   C-side, emit changed subtrees. Design-heavy; file under future.

Not worth doing: buffering `wire_write` (0.08 s), compressing the dump
(fixes bytes, not the time spent producing them), or hashing tracked
values OCaml-side (physical identity of moving, mutating blocks is
exactly what you can't hash — the C side can, against GC counters).

## Reproducing

The rig reuses `canary.sh`'s artifacts read-only: copy
`vreplay/src/*.ml*` to a scratch dir, patch, compile with the fork's
installed `ocamlc.byte`, link `vreplay.cma` (`-dllib -lvreplaybyt
-cclib -lvreplaybyt`, reusing the built `libvreplaybyt.a`), symlink-
clone `_vreplay/.toolchain/ocamllib` with `vreplay/` swapped for the
scratch build, then `dune build --root <exchange-tree> --build-dir
<scratch>` under that `OCAMLLIB` with the toolchain's shims on PATH.
Two traps: dune's shared cache will silently restore a stale link that
predates the `vreplay.cma` swap (`DUNE_CACHE=disabled`, and `+vreplay`
is invisible to dune's dependency graph, so touch the program when in
doubt), and only user CPU time is comparable on a shared box.

Field/timing analyzers and the verification fold are small Python
scripts kept with the prototype; the byte breakdown needs nothing but
the dump. Interface load times came from a 10-line scratch executable
timing `Dump_reader.read` → `Call_stack.create` → `Replay.create`.
