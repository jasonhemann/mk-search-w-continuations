# Project Worklist

Last updated: 2026-03-04

This is the pull queue for independently-executable tasks.

Status values:
- `todo`: ready to pull
- `doing`: currently in progress
- `blocked`: waiting on a prior decision
- `done`: finished and merged

## Recent Completions (Already Merged)

- Archive workstreams were renamed to purpose-first names and documented in per-subdir READMEs.
- Historical timeline text was consolidated into archive docs; redundant redirect file removed.
- `interp-1.rkt` / `interp-2.rkt` were documented as seminal branch-point artifacts (historical-foundational, non-gating).
- Stale compile-status claims in supporting docs were corrected for files that now compile.
- `WL-004` was closed: canonical regression gate command is documented in `README.md` and points to `new-derivation/derivation-invariant-regressions.rkt`.

## Current Compile Snapshot (2026-03-04)

Repo-wide `raco make` over `*.rkt` currently reports `8` failing files:

1. `sk-fk-from-double-cps/deep-remove-first-1.rkt` (`sk` unbound)
2. `new-derivation/experiments/nested-runs.rkt` (syntax error: missing `)`)
3. `new-derivation/shallow-monadic-interp-1.rkt` (`bind` unbound)
4. `old-mk-cps/three-k-test.rkt` (`c` unbound)
5. `old-mk-cps/mk-monad-map-join.rkt` (`ma?` unbound)
6. `w-michael.rkt` (identifier redefined)
7. `interp/run-interp.rkt` (`?k` unbound)
8. `interp-2.rkt` (missing `monads.rkt` dependency)

## Pull Order (Suggested: Low-Decision, Real-Win First)

1. `WL-009` Old-code salvage-first pass (`old-mk-cps` and `archive`)
2. `WL-003` Unbound-identifier backlog closure
3. `WL-007` Documentation synchronization pass
4. `WL-002` Code-path bug-fix shortlist and fix specs
5. `WL-001` Sus backlog policy decisions (non-code artifacts)
6. `WL-005` Paper build reproducibility/toolchain
7. `WL-006` Bibliography intake policy for non-DOI items
8. `WL-008` Monad-law equivalence and proof-shape decision (`blocked`)

## Open Tasks

### WL-009 Old-code salvage-first pass (`old-mk-cps` vs `archive`)
- Status: `doing`
- Scope:
  - `old-mk-cps/`
  - `archive/`
- Goal: classify each remaining failure as one of `salvage-now`, `archive-only`, or `external-dependency`.
- Definition of done:
  - Each current `raco make` failure in `old-mk-cps/` and `archive/` has a chosen disposition.
  - Salvage-now files are either made runnable via explicit stage modules with runnable probes, or explicitly moved out of active salvage scope.
  - Directory policy is documented in `README.md` and `archive/README.md`.

### WL-003 Unbound-identifier backlog closure
- Status: `doing`
- Scope:
  - `unbound-identifiers-triage.md`
  - current failing files with unbound names
- Goal: resolve each row by one of `fix`, `derivation-step-module`, `archive-only`, `external-dependency`.
- Definition of done:
  - No unresolved unbound identifier without a chosen disposition.
  - Triage doc is synchronized to the current failure set.

### WL-007 Documentation synchronization pass
- Status: `doing`
- Scope:
  - `README.md`
  - `mk-search-structure-audit.md`
  - `stage-relative-failure-oracle-report.md`
  - `unbound-identifiers-triage.md`
  - `imported-files-and-sus-worklist.md`
  - `worklist.md`
- Goal: remove stale contradictions and align current status language.
- Definition of done:
  - No contradictory status claims across the listed docs.
  - README links and labels match current filenames and roles.

### WL-002 Code-path bug-fix shortlist and fix specs
- Status: `todo`
- Scope:
  - v4 sk/fk `run` parity bug from `stage-relative-failure-oracle-report.md`
- Goal: convert open bug(s) into explicit fix specs (expected behavior + tests + acceptance checks).
- Definition of done:
  - For each bug: written expected behavior, minimal repro, exact files/functions to edit, and regression tests to add.

### WL-001 Sus backlog policy decisions
- Status: `todo`
- Scope:
  - `imported-files-and-sus-worklist.md`
  - `new-derivation/*` experimental/scratch files called out as `sus`
- Goal: decide `promote` vs `archive-as-context` for each `sus` entry.
- Definition of done:
  - Every `sus` row has explicit disposition (`promote`, `archive`, `keep-scratch`).
  - Each row has one next action and owner rationale.

### WL-005 Paper build reproducibility/toolchain
- Status: `todo`
- Scope:
  - `paper/Makefile`, TeX/Biber compatibility, reproducible build command
- Goal: establish a known-good, documented build path in current environment.
- Definition of done:
  - `paper` build instructions run end-to-end on this repo state.
  - Required tool versions or constraints are documented.

### WL-006 Bibliography intake policy for non-DOI items
- Status: `todo`
- Scope:
  - no-DOI files noted in `paper/streams.bib` comments
- Goal: decide whether/how non-DOI works are included (e.g., `@misc` rules, required metadata floor).
- Definition of done:
  - Written policy in `paper/streams.bib` comment header or separate note.
  - Existing non-DOI candidates either normalized or explicitly deferred.

### WL-008 Monad-law equivalence and proof-shape decision
- Status: `blocked`
- Scope:
  - `new-derivation/proofs/monad-laws.rkt`
  - `new-derivation/proofs/third-monad-law.rkt`
  - `new-derivation/proofs/proof-obligations-checklist.md`
- Goal: fix the theorem target and proof architecture before additional derivation work.
- Blocking decisions:
  - Select equality notion: extensional vs CBV observational (`run`/fuel).
  - Specify assumed versus proved `mplus/mzero` laws.
  - Choose one bind presentation (`bind` arity and vc story) or write a correspondence lemma.
- Definition of done:
  - Theorem statements are rewritten against the chosen equivalence.
  - Dependency lemmas are listed explicitly with assumption/proof status.
  - `third-monad-law.rkt` sketch is either completed or replaced by a structured obligation list.

## Closed Tasks

### WL-004 Active regression-gate boundary
- Status: `done`
- Outcome:
  - Canonical regression gate command is documented in `README.md`.
  - Gate file is `new-derivation/derivation-invariant-regressions.rkt`.
  - Exploratory files are documented as non-gating context.
