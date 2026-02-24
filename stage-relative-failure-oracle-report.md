# Stage-Relative Failure Oracle Report (Derivations 1–4)

Status date: February 24, 2026.

This document supersedes the earlier pre-fix oracle snapshot and distinguishes:
- what is now fixed,
- what remains open,
- and what is stage-intended divergence.

## Critical Findings (Current Open Issues)

1. **[BUG] v4 sk/fk `run` remains parity-divergent from stream peers in shared run scenarios.**
   - Stream modules produce:
     - `run/productive => (5 5 5)`
     - `run/interleave-prefix => (5 6 5 6)`
     - `run/interleave-count => 49`
   - v4 sk/fk modules currently produce:
     - `run/productive => (5 5)`
     - `run/interleave-prefix => (5 6 5)`
     - `run/interleave-count => 48`
   - Sources:
     - [mk-streams-derivation-4.rkt:155](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-4.rkt:155)
     - [mk-streams-derivation-4.rkt:184](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-4.rkt:184)
     - [mk-streams-derivation-4.rkt:262](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-4.rkt:262)

## Major Findings (Resolved Since Initial Oracle Pass)

1. **[RESOLVED] v1 delayed `bind` path in `streams-bind-return` is fixed.**
   - Source:
     - [mk-streams-derivation-1.rkt:69](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-1.rkt:69)

2. **[RESOLVED] delayed `map` in `streams-unit-map-join` across v1–v4 now produces proper stream values.**
   - Sources:
     - [mk-streams-derivation-1.rkt:30](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-1.rkt:30)
     - [mk-streams-derivation-2.rkt:20](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-2.rkt:20)
     - [mk-streams-derivation-3.rkt:31](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-3.rkt:31)
     - [mk-streams-derivation-4.rkt:55](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-4.rkt:55)

3. **[RESOLVED] v2/v3 sk/fk `dk` threading loss in `join`/`bind` is fixed.**
   - Sources:
     - [mk-streams-derivation-2.rkt:131](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-2.rkt:131)
     - [mk-streams-derivation-2.rkt:162](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-2.rkt:162)
     - [mk-streams-derivation-3.rkt:142](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-3.rkt:142)
     - [mk-streams-derivation-3.rkt:179](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-3.rkt:179)

4. **[RESOLVED] v2/v3 sk/fk `core/mapjoin-vs-bind` regressions are fixed.**
   - Sources:
     - [mk-streams-derivation-2.rkt:138](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-2.rkt:138)
     - [mk-streams-derivation-2.rkt:153](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-2.rkt:153)
     - [mk-streams-derivation-3.rkt:149](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-3.rkt:149)
     - [mk-streams-derivation-3.rkt:170](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-3.rkt:170)

5. **[RESOLVED] `examples-new.rkt` stale `l/m` expectation is fixed.**
   - Source:
     - [examples-new.rkt:46](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/examples-new.rkt:46)

6. **[RESOLVED] `examples-suite-2.rkt` (formerly `stream-fix-mplus-bind-examples.rkt`) now compiles.**
   - Source:
     - [examples-suite-2.rkt:89](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/examples-suite-2.rkt:89)

## Expected Divergences

1. **[INTENDED-DIFF] Stage growth remains intentional.**
   - v1: no `dk`, no `define-relation`, no `run`.
   - v2: introduces 3k `dk`, still pre-`define-relation`, pre-`run`.
   - v3: introduces `define-relation`, still pre-`run`.
   - v4: introduces `run`.

2. **[INTENDED-DIFF] Observer presentation differences in v3 productive delay scenarios are acceptable when semantics agree.**

## Stage Gaps

1. **[STAGE-GAP] `run` before v4.**
2. **[STAGE-GAP] `define-relation` before v3.**
3. **[STAGE-GAP] 3k `dk` semantics in v1.**

## Correct Behavior Spec by Version (Updated)

### v1
- finite monad behavior coherent across stream/sk-fk baseline.
- delayed branches must progress (no self-recursive delay stutter).

### v2
- preserve baseline finite behavior.
- preserve `dk` through `join`/`bind` in `dk`-sensitive probes.

### v3
- preserve v2 `dk` obligations.
- `define-relation` supports guarded recursive delay.

### v4
- preserve v3 behavior.
- `run` should expose consistent chain/productive/unproductive/interleave semantics across stream and sk/fk modules for shared scenarios.

## Matrix (Current Open Rows)

| file  | module                | scenario                | expected    | actual         | classification | severity | reference                                                                                                                                          |
|-------|-----------------------|-------------------------|-------------|----------------|----------------|----------|----------------------------------------------------------------------------------------------------------------------------------------------------|
| `-4`  | `sk/fk-unit-map-join` | `run/productive`        | `(5 5 5)`   | `(5 5)`        | `BUG`          | High     | [mk-streams-derivation-4.rkt:184](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-4.rkt:184)               |
| `-4`  | `sk/fk-bind-return`   | `run/productive`        | `(5 5 5)`   | `(5 5)`        | `BUG`          | High     | [mk-streams-derivation-4.rkt:262](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-4.rkt:262)               |
| `-4`  | `sk/fk-unit-map-join` | `run/interleave-prefix` | `(5 6 5 6)` | `(5 6 5)`      | `BUG`          | High     | [mk-streams-derivation-4.rkt:184](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-4.rkt:184)               |
| `-4`  | `sk/fk-bind-return`   | `run/interleave-prefix` | `(5 6 5 6)` | `(5 6 5)`      | `BUG`          | High     | [mk-streams-derivation-4.rkt:262](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-4.rkt:262)               |
| `-4`  | `sk/fk-unit-map-join` | `run/interleave-count`  | `49`        | `48`           | `BUG`          | High     | [mk-streams-derivation-4.rkt:184](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-4.rkt:184)               |
| `-4`  | `sk/fk-bind-return`   | `run/interleave-count`  | `49`        | `48`           | `BUG`          | High     | [mk-streams-derivation-4.rkt:262](/Users/jhemann/Documents/mk-search-w-continuations/new-derivation/mk-streams-derivation-4.rkt:262)               |

## Current Fix Shortlist

1. Align v4 sk/fk `run` behavior with stream peer behavior for shared scenarios.

## Validation Evidence (Post-Fix)

- `examples-new.rkt` now runs without failures.
- `examples-suite-2.rkt` now compiles after quarantining non-runnable scratch reductions.
- Oracle probes now show:
  - `core/map-delay => (2 3 4)` across stream modules v1–v4.
  - `dk/join-unit => ((5) (5) #t)` in v2/v3/v4 sk/fk modules.
  - `core/mapjoin-vs-bind => ... #t` across all scoped modules.
- Remaining mismatch is isolated to v4 sk/fk `run` scenarios listed above.
