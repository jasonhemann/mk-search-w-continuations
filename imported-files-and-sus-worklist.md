# Imported Files and Sus Worklist

This note records two things:

1. Which files were imported from `/Users/jhemann/class/2021/Summer/mk/interp`.
2. Which currently staged files are "sus" (weak, incomplete, or broken) and how to treat them.

## Imported from `/Users/jhemann/class/2021/Summer/mk/interp`

| current file                                     | source file                                                                    | why it belongs here now                                                        | current status                                                   |
|--------------------------------------------------|--------------------------------------------------------------------------------|--------------------------------------------------------------------------------|------------------------------------------------------------------|
| `interp/mk-backtracking-without-unify.rkt`       | `/Users/jhemann/class/2021/Summer/mk/interp/mk-backtracking-without-unify.rkt` | Core interpreter/backtracking work; logically belongs under `interp/`.         | Fails compile: bad module header (`GG#lang` typo at file start). |
| `new-derivation/proofs/monad-laws.rkt`           | `/Users/jhemann/class/2021/Summer/mk/interp/monad-laws.rkt`                    | Monad-law reasoning artifact; belongs with derivation proofs.                  | Compiles.                                                        |
| `new-derivation/proofs/third-monad-law.rkt`      | `/Users/jhemann/class/2021/Summer/mk/interp/third-monad-law.rkt`               | Companion proof sketch for associativity work.                                 | Fails compile: `bind` unbound at top-level (expects context).    |
| `new-derivation/experiments/cps-xform-monad.rkt` | `/Users/jhemann/class/2021/Summer/mk/interp/cps-xform-monad.rkt`               | CPS transformation experiment, clearly exploratory; belongs under experiments. | Compiles.                                                        |
| `new-derivation/experiments/nested-runs.rkt`     | `/Users/jhemann/class/2021/Summer/mk/interp/nested-runs.rkt`                   | Nested-run design/probe examples; exploratory file.                            | Fails compile: syntax error (missing `)` near line 29).          |

Note: `interp/new-backtrack-interp-env-passing.rkt` is a repo-internal rename (from `run-interp/new-backtrack-interp-env-passing.rkt`), not an external import.

## Sus files

These are reasonable to keep in VC for context, but should not be treated as regression gates or polished modules.

| file                                                       | why sus                                                                  | objective signal                                                    | recommended next action                                                                                    |
|------------------------------------------------------------|--------------------------------------------------------------------------|---------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------|
| `interp/mk-backtracking-without-unify.rkt`                 | Structural typo at module boundary.                                      | `raco make` fails: expected module declaration.                     | Keep tracked; fix trivial `#lang` typo in a dedicated cleanup commit.                                      |
| `interp/run-interp.rkt`                                    | Stub with placeholder continuation names.                                | `raco make` fails: `?k` unbound (line 15).                          | Keep tracked as skeleton; mark clearly as incomplete or finish contract before using.                      |
| `new-derivation/shallow-monadic-interp-1.rkt`              | Partial derivation with unresolved expression paths.                     | `raco make` fails: `bind` unbound (line 74).                        | Keep tracked as derivation scratch; either complete or comment out broken path.                            |
| `new-derivation/experiments/nested-runs.rkt`               | Draft examples contain malformed form(s).                                | `raco make` fails: read-syntax missing `)`.                         | Keep tracked in `experiments/`; fix syntax or convert to note-only block comments.                         |
| `new-derivation/proofs/third-monad-law.rkt`                | Proof text encoded as top-level code without imports/context.            | `raco make` fails: `bind` unbound (line 3).                         | Keep tracked as proof note; either add explicit scaffolding or convert to `.md`/commented derivation form. |
| `w-michael.rkt`                                            | Large exploratory hybrid with duplicate defs and unresolved transitions. | `raco make` fails: `$append` already defined (around line 103).     | Keep tracked as historical exploration; split salvageable pieces into focused files later.                 |
| `new-derivation/basic-shallow-monadic-interp.rkt`          | Very small probe; low confidence as standalone asset.                    | Compiles, but only demonstrates a minimal `amb/fail` toy evaluator. | Keep tracked as tiny experiment; add one-line purpose header or fold into experiments directory later.     |
| `new-derivation/construct-examples-to-test-deductions.rkt` | Utility generator feels ad hoc without surrounding test harness.         | Compiles; emits generated strings, no assertions.                   | Keep tracked; either integrate into test flow or tag explicitly as throwaway generator.                    |

## Operating rule for now

- Keep these files versioned for historical and design context.
- Do not require them to pass as part of the active derivation regression gate until promoted.
- Promote file-by-file only when each has a clear purpose statement and compile/runtime expectation.
