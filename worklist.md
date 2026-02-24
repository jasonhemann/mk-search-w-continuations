# Project Worklist

Last updated: 2026-02-24

This is the pull queue for independently-executable tasks.

Status values:
- `todo`: ready to pull
- `doing`: currently in progress
- `blocked`: waiting on a prior decision
- `done`: finished and merged

## Pull Order (Suggested)

1. `WL-001` Sus backlog policy decisions (non-code artifacts)
2. `WL-002` Code-path bug-fix shortlist and fix specs
3. `WL-003` Unbound-identifier backlog closure
4. `WL-004` Active regression-gate boundary
5. `WL-005` Paper build reproducibility/toolchain
6. `WL-006` Bibliography intake policy for non-DOI items
7. `WL-007` Documentation synchronization pass

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
