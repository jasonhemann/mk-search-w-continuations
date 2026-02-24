# Unbound Identifier Triage

## Scope and method

This triage is based on a repo-wide compile sweep using `raco make` over all `*.rkt` files (excluding editor backup patterns).

## Fixed in this change

- File: `old-mk-cps/a9-skmonads/map-join-xform-other-monad-4.rkt`
- Location: line 39
- Identifier previously reported unbound: `dk^`
- Fix: corrected `match` quasiquote binder pattern from `` `(loop-base-dk dk^ sk fk) `` to `` `(loop-base-dk ,dk^ ,sk ,fk) `` so symbols are bound rather than matched literally.
- Rationale: this is a pattern-binding correctness fix only, not an algorithmic change.
- Post-fix note: the same file still contains a separate placeholder call at line 73 (`<n>`, `<exp>`, `<dk>`, `<sk>`, `<fk>`, `<k>`) that remains unresolved by design in this pass.

- File: `new-derivation/examples-suite-2.rkt` (renamed from `new-derivation/stream-fix-mplus-bind-examples.rkt`)
- Prior location: line 105 in the old filename
- Identifier previously reported unbound: `kons`
- Fix: quarantined non-runnable reduction scratch in a block comment so the suite file now compiles while retaining derivation notes.
- Rationale: this preserves experimental reasoning text without treating scratch fragments as executable test code.

## Remaining unbound identifiers

| file | line | identifier | classification | recommended next action |
|---|---:|---|---|---|
| `sk-fk-from-double-cps/deep-remove-first-1.rkt` | 95 | `sk` | historical-experimental | Treat as derivation scratch; either define/rename continuations consistently or move non-runnable fragment into comments. |
| `new-derivation/shallow-monadic-interp-1.rkt` | 74 | `bind` | incomplete/stub | Complete the partially written expression with concrete continuation arguments and intended binder. |
| `old-mk-cps/test-monads.rkt` | 4 | `writer-log` | requires intent | Align callsite with `monads-struct.rkt` API (`writer` struct accessors or `run-writer`) and pick one canonical writer test style. |
| `old-mk-cps/a9-skmonads/map-join-xform-other-monad-4d.rkt` | 48 | `c` | historical-experimental | Inspect intended continuation parameter threading; add missing binder or rename to existing in-scope variable. |
| `old-mk-cps/a9-skmonads/map-join-xform-other-monad-4.rkt` | 73 | `<n>` | incomplete/stub | Replace/remove placeholder top-level probe call (`<n>`, `<exp>`, `<dk>`, `<sk>`, `<fk>`, `<k>`) once intended invocation is specified. |
| `old-mk-cps/a9-skmonads2/micro-ks-3.rkt` | 49 | `sk` | incomplete/stub | Replace free `sk/fk/dk` with explicit parameters in `init-dk` or lexical bindings. |
| `old-mk-cps/two-k-test.rkt` | 8 | `a?` | requires intent | Choose/define intended discriminator predicate for answer-vs-failure payload. |
| `old-mk-cps/map-join-xform-other-monad.rkt` | 6 | `bind` | incomplete/stub | Import/define the target monad operators (`bind`, `unit`, `fail`, `mdelay`) before `ee`. |
| `old-mk-cps/map-join-with-dan.rkt` | 101 | `bind` | historical-experimental | Re-enable intended `bind` definition or convert the bottom examples into comments if file is note-only. |
| `run-interp/run-interp.rkt` | 15 | `?k` | incomplete/stub | Replace placeholder continuation names (`?k`, `?k2`) with concrete parameters and complete `valof^-cps` contract. |
