# Unbound Identifier and Compile-Failure Triage

## Scope and method

This triage is synchronized to a repo-wide compile sweep (`raco make` over all `*.rkt`, excluding editor backup patterns) run on **2026-03-04**.

## Current failing files (2026-03-04)

The current sweep reports `8` failing files. This table is the active backlog for `WL-003`.

| file | primary failure | classification | recommended next action |
|---|---|---|---|
| `sk-fk-from-double-cps/deep-remove-first-1.rkt` | `sk` unbound (line 95) | historical-experimental | Decide `salvage-now` vs `archive-only`; if salvaged, normalize continuation names and bind all continuation arguments explicitly. |
| `new-derivation/experiments/nested-runs.rkt` | read-syntax missing `)` (line 29) | trivial-syntax + requires-intent | Apply minimal parenthesis fix, then either add required dependencies for `run`/`==`/`typo` or mark the file as note-only exploratory context. |
| `new-derivation/shallow-monadic-interp-1.rkt` | `bind` unbound (line 74) | incomplete/stub | Complete the partially written expression path and bind to the intended monad interface for this file’s stage. |
| `old-mk-cps/three-k-test.rkt` | `c` unbound (line 48) | salvageable-derivation | Convert to explicit stage modules with runnable probes; remove accidental cross-stage free references. |
| `old-mk-cps/mk-monad-map-join.rkt` | `ma?` unbound (line 67) | salvageable-derivation | Resolve representation discriminator (`ma?`) by defining it or replacing with the intended stream/computation predicate, then validate probes. |
| `w-michael.rkt` | identifier already defined (`$append`) | salvageable-derivation | Split derivation stages into `module stepN racket` blocks and keep each stage internally consistent with local probes. |
| `interp/run-interp.rkt` | `?k` unbound (line 15) | incomplete/stub | Replace placeholder continuation names (`?k`, `?k2`) with concrete contract-level parameters, or explicitly classify as historical skeleton. |
| `interp-2.rkt` | cannot open module file (`monads.rkt`) | external-dependency | Either recover/add a compatible local `monads.rkt` shim, or keep as historical-foundational non-runnable artifact with explicit note. |

## Resolved/removed from active failure set

These were previously listed as unresolved in this doc and are no longer in the current failure set:

- `old-mk-cps/test-monads.rkt` (`writer-log` unbound) — now aligned to current API.
- `archive/defunctionalized-dispatch-experiments/map-join-xform-other-monad-4d.rkt` (`c` unbound) — no longer reported by current sweep.
- `archive/defunctionalized-dispatch-experiments/map-join-xform-other-monad-4.rkt` placeholder top-level `<...>` invocation — no longer reported by current sweep.
- `archive/micro-language-3k-derivation/micro-ks-3.rkt` (`sk` unbound) — no longer reported by current sweep.
- `archive/map-join-transformations/map-join-xform-other-monad.rkt` (`bind` unbound) — no longer reported by current sweep.
- `archive/three-continuation-derivation/map-join-with-dan.rkt` (`bind` unbound) — no longer reported by current sweep.

## Notes

- This document tracks compile failures, not semantic correctness.
- For derivation files, preferred remediation is **explicit staged modules with runnable probes**, not broad comment-out suppression.
