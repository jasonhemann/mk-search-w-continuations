# mK Search Structure Audit (Deprecated-Biased, No PDF Content)

Status date: February 24, 2026.

## Scope and weighting

This audit excludes all PDF contents and excludes backup/editor/build artifacts.

### Full-weight files (active analysis)
- `/Users/jhemann/Documents/mk-search-w-continuations/README.md`
- `/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation.rkt`
- `/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-1.rkt`
- `/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-2.rkt`
- `/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-3.rkt`
- `/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-4.rkt`
- `/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/examples.rkt`
- `/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/examples-new.rkt`
- `/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/examples-suite-2.rkt`
- `/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/uncurried-impl.rkt`
- `/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/continuation-derivation-notes.org`
- `/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/continuation-case-matrix.txt`

### Low-weight files (historical context)
- `/Users/jhemann/Documents/mk-search-w-continuations/old-mk-cps/research-story.txt`
- `/Users/jhemann/Documents/mk-search-w-continuations/archive/three-continuation-derivation/README.md`
- `/Users/jhemann/Documents/mk-search-w-continuations/old-mk-cps/dir-listing-work-order.txt`

## Deprecation-aware map (current)

### Active
- `new-derivation/mk-streams-derivation.rkt`
- `new-derivation/examples.rkt`
- `new-derivation/derivation-invariant-regressions.rkt`

Rationale: these establish the strongest current baseline and include direct regression coverage for recently fixed monad-law paths.

### Experimental
- `new-derivation/mk-streams-derivation-4.rkt`
- `new-derivation/examples-new.rkt`
- `new-derivation/examples-suite-2.rkt`
- `new-derivation/uncurried-impl.rkt`

Rationale: useful exploration artifacts. `examples-new.rkt` now runs cleanly, `examples-suite-2.rkt` now compiles with non-runnable scratch reductions quarantined, but `-4` sk/fk `run` parity remains open.

### Historical
- `new-derivation/mk-streams-derivation-1.rkt`
- `new-derivation/mk-streams-derivation-2.rkt`
- `new-derivation/mk-streams-derivation-3.rkt`
- `new-derivation/continuation-derivation-notes.org`
- `new-derivation/continuation-case-matrix.txt`
- `old-mk-cps/*.txt`

Rationale: they preserve derivation intent and reasoning context but are not primary implementation authority.

### Deprecated (implementation authority)
- `old-mk-cps/*` implementation streams.

Rationale: these are high-friction research trails with unresolved placeholders and frequent partial derivations.

## Canonical control-flow model

### Stream variants (`streams-unit-map-join`, `streams-bind-return`)
- Representation: odd streams (`cons` / delayed tail promise / `null`).
- `define-relation` introduces delay boundaries.
- `run` has full and fuel-bounded modes.
- `mplus` interleaves by flipping delayed-left branches.
- `bind` and `map` now agree on delayed-tail progression.

### Continuation variants (`sk/fk-unit-map-join`, `sk/fk-bind-return`)
- Representation: 3k computations `dk/sk/fk`.
- `define-relation` re-enters through `dk`.
- `mplus` threads residual search through `sk`.
- `bind` / `join` must preserve `dk` when evaluating continuation-valued results.

## Operator contract table and invariants

| Operator          | Contract                           | Invariant                                                              |
|-------------------|------------------------------------|------------------------------------------------------------------------|
| `run`             | `(run m)` or `(run n m)`           | Fuel behavior should align across stream/sk-fk for shared scenarios.   |
| `define-relation` | relation-level delayed suspension  | Recursive relations remain guarded.                                    |
| `unit`/`return`   | singleton success                  | No hidden branching or fuel consumption.                               |
| `mzero`           | empty computation                  | Identity for `mplus`.                                                  |
| `mplus`           | interleaving choice                | Delayed-left branch must not starve right branch.                      |
| `bind`            | `Ma -> (a -> Mb) -> Mb`            | Must preserve immediate and delayed branches.                          |
| `map`/`join`      | equivalent view of `bind`/`return` | No extra flattening in `map`; `join` does flattening only where typed. |

## Current behavior validation

### Passing checks
- `raco test new-derivation/examples.rkt`: passes (`21 tests passed`).
- `racket new-derivation/examples-new.rkt`: passes (no failures printed).
- `racket new-derivation/derivation-invariant-regressions.rkt`: passes (`14 success(es), 0 failure(s)`).
- `raco make new-derivation/examples-suite-2.rkt`: passes (no unbound identifiers).

### Parity matrix (current high-signal scenarios)

| File/module                                         | Productive recursion | Interleave prefix | Interleave count | Status   |
|-----------------------------------------------------|---------------------:|------------------:|-----------------:|----------|
| `mk-streams-derivation.rkt` streams-unit-map-join   |            `(5 5 5)` |       `(5 6 5 6)` |             `49` | baseline |
| `mk-streams-derivation.rkt` streams-bind-return     |            `(5 5 5)` |       `(5 6 5 6)` |             `49` | baseline |
| `mk-streams-derivation.rkt` sk/fk-unit-map-join     |            `(5 5 5)` |       `(5 6 5 6)` |             `49` | baseline |
| `mk-streams-derivation.rkt` sk/fk-bind-return       |            `(5 5 5)` |       `(5 6 5 6)` |             `49` | baseline |
| `mk-streams-derivation-4.rkt` streams-unit-map-join |            `(5 5 5)` |       `(5 6 5 6)` |             `49` | aligns   |
| `mk-streams-derivation-4.rkt` streams-bind-return   |            `(5 5 5)` |       `(5 6 5 6)` |             `49` | aligns   |
| `mk-streams-derivation-4.rkt` sk/fk-unit-map-join   |              `(5 5)` |         `(5 6 5)` |             `48` | open bug |
| `mk-streams-derivation-4.rkt` sk/fk-bind-return     |              `(5 5)` |         `(5 6 5)` |             `48` | open bug |

## Bugs fixed since prior draft

1. Delayed `map` fixed in `streams-unit-map-join` lineage (v1–v4).
2. v1 `streams-bind-return` delayed `bind` arity/path fixed.
3. v2/v3 sk/fk `dk` threading in `join`/`bind` fixed.
4. v2/v3 `core/mapjoin-vs-bind` regressions fixed.
5. `examples-new.rkt` stale `l/m` expectation corrected.

## Open issues that still need repair

1. v4 sk/fk `run` parity mismatch against stream peers.
2. `examples-suite-2.rkt` still contains quarantined scratch reductions that are intentionally non-executable.

## Reframed failed ideas (notes to self)

Use these as “try/fail/explain” reminders for future passes.

1. **You might think:** put `join` into delayed `map` branches to “normalize” structure.  
   **Why that fails:** `map` is not a flattening operator; this changes monadic depth and yields non-stream artifacts.  
   **Better framing:** keep `map` shape-preserving; reserve flattening for `join`/`bind`.

2. **You might think:** in 3k code, success/failure continuations are enough when resuming `mb`.  
   **Why that fails:** dropping `dk` causes residual procedures to leak and breaks `join(unit mb) == mb` observationally.  
   **Better framing:** treat `dk` as mandatory context propagation, not optional plumbing.

3. **You might think:** once core monad ops match, `run` parity is automatic.  
   **Why that fails:** answer extraction/fuel accounting is an additional semantic layer and can diverge independently.  
   **Better framing:** maintain a separate `run` parity test matrix from core monad-law checks.

4. **You might think:** scratch reduction blocks can stay executable in the same file as tests.  
   **Why that fails:** free identifiers (`kons`, placeholders) break compile/test and hide signal from real checks.  
   **Better framing:** split runnable checks from scratch derivations; comment or isolate the latter.

5. **You might think:** stale expected values in exploratory tests are harmless.  
   **Why that fails:** they create false negatives and can send debugging effort down dead ends.  
   **Better framing:** tie expectations to a named module target and keep parity probes automated.

## Decision output (updated)

### Still makes sense to work on
- Keep `mk-streams-derivation.rkt` as semantic reference.
- Continue regression-first workflow with `derivation-invariant-regressions.rkt`.
- Repair v4 sk/fk `run` to restore scoped parity.

### Needs repair before trust
- `mk-streams-derivation-4.rkt` sk/fk `run` behavior.
- `examples-suite-2.rkt` cleanup if you want the entire file runnable without quarantined scratch blocks.

### No longer worth active investment
- Treating `old-mk-cps` implementation branches as active repair targets unless a specific historical artifact is required.
