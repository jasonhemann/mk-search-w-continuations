# Project Worklist

Last updated: 2026-02-25

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

## Pull Order (Suggested)

1. `WL-001` Sus backlog policy decisions (non-code artifacts)
2. `WL-009` Old-code placement decision (`old-mk-cps` vs `archive`)
3. `WL-002` Code-path bug-fix shortlist and fix specs
4. `WL-003` Unbound-identifier backlog closure
5. `WL-004` Active regression-gate boundary
6. `WL-005` Paper build reproducibility/toolchain
7. `WL-006` Bibliography intake policy for non-DOI items
8. `WL-007` Documentation synchronization pass
9. `WL-008` Monad-law equivalence and proof-shape decision

## Tasks

### WL-001 Sus backlog policy decisions
- Status: `todo`
- Scope:
  - `imported-files-and-sus-worklist.md`
  - `new-derivation/*` experimental/scratch files called out as `sus`
- Goal: decide `promote` vs `archive-as-context` for each `sus` entry.
- Definition of done:
  - Every `sus` row has explicit disposition (`promote`, `archive`, `keep-scratch`).
  - Each row has one next action and owner rationale.

### WL-009 Old-code placement decision (`old-mk-cps` vs `archive`)
- Status: `todo`
- Scope:
  - `old-mk-cps/`
  - `archive/`
- Goal: decide whether `old-mk-cps` remains a top-level historical subtree or is folded under `archive/`, and classify each failing file as `salvage`, `archive-only`, or `generated-detritus`.
- Definition of done:
  - Directory policy is explicitly documented in `README.md` and `archive/README.md`.
  - Each current `raco make` failure in `old-mk-cps/` and `archive/` has a chosen disposition.
  - Generated artifacts (`compiled/`, TeX aux files, `.DS_Store`) have an explicit keep/remove rule.

### WL-002 Code-path bug-fix shortlist and fix specs
- Status: `todo`
- Scope:
  - v4 sk/fk `run` parity bug from `stage-relative-failure-oracle-report.md`
- Goal: convert open bug(s) into explicit fix specs (expected behavior + tests + acceptance checks).
- Definition of done:
  - For each bug: written expected behavior, minimal repro, exact files/functions to edit, and regression tests to add.

### WL-003 Unbound-identifier backlog closure
- Status: `todo`
- Scope:
  - Remaining rows in `unbound-identifiers-triage.md`
- Goal: resolve each row by one of `fix`, `comment/quarantine`, `archive-only`.
- Definition of done:
  - No unresolved rows without a chosen disposition.
  - Triage file updated with decisions and follow-up tickets where needed.

### WL-004 Active regression-gate boundary
- Status: `todo`
- Scope:
  - `new-derivation/derivation-invariant-regressions.rkt`
  - exploratory suites (`examples-suite-2.rkt`, experiments, scratch files)
- Goal: define what is authoritative gate vs non-gating exploration.
- Definition of done:
  - One canonical gate command documented in README.
  - Non-gating files labeled clearly as exploratory.

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

### WL-007 Documentation synchronization pass
- Status: `todo`
- Scope:
  - `README.md`
  - `mk-search-structure-audit.md`
  - `stage-relative-failure-oracle-report.md`
  - `unbound-identifiers-triage.md`
  - `imported-files-and-sus-worklist.md`
- Goal: remove stale contradictions and align current status language.
- Definition of done:
  - No contradictory status claims across the listed docs.
  - README links and labels match current filenames and roles.

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
