# Monad Laws Proof Obligations Checklist

Last updated: 2026-02-24

Purpose:
- make proof work explicit, staged, and reviewable
- separate proven results from assumptions and tested evidence
- choose a proof approach that fits current collaborators

## Decision Lock (Before More Derivation)

- [ ] Choose theorem equality: extensional equality vs CBV observational equivalence.
- [ ] If observational: freeze observer definition (`run`, fuel, answer order policy).
- [ ] Freeze fairness/interleaving policy in writing.
- [ ] Freeze `bind` presentation: 1-arg, 2-arg (`vc`), or correspondence theorem between both.

## Core Theorem Targets

- [ ] Left identity: `return a >>= f ≈ f a`
- [ ] Right identity: `m >>= return ≈ m`
- [ ] Associativity: `(m >>= g) >>= h ≈ m >>= (λx. g x >>= h)`

Where `≈` is the chosen equality above.

## Dependency Lemmas

- [ ] `mplus` left identity: `mplus mzero m ≈ m`
- [ ] `mplus` right identity: `mplus m mzero ≈ m`
- [ ] `mplus` associativity (or exact scoped variant if fairness weakens full associativity)
- [ ] `bind mzero f ≈ mzero`
- [ ] `bind`/`mplus` distribution law(s), with scope conditions if required
- [ ] Delay forcing/commutation lemmas needed for recursive cases

## Evidence Discipline

- [ ] For each theorem/lemma, mark status as `proved`, `assumed`, or `tested-only`.
- [ ] For each `tested-only` claim, list the test file and scenario name.
- [ ] Keep one "Known limits" section documenting unproved assumptions.

## Approach Selection (Choose One Primary, Optional Secondary)

### 1) Bisimulation-first
- Best when:
  - codata behavior and continuation stepping are central
  - collaborator strength is semantics/coinduction
- Deliverable:
  - a bisimulation relation with closure lemmas for `bind`, `mplus`, `delay`

### 2) Step-indexed logical relations
- Best when:
  - direct coinduction gets stuck on mixed data/codata and recursive bind
  - collaborator strength is PL metatheory/logical relations
- Deliverable:
  - index-parametric relation proving laws at every fuel/index

### 3) Translation/correspondence to a canonical model
- Best when:
  - you already trust a stream or continuation model with known properties
  - collaborator strength is program transformation or model correspondence
- Deliverable:
  - semantics-preserving translation + transport of laws through correspondence

### 4) Mechanized core first (small, high-value fragment)
- Best when:
  - the team needs early confidence and ambiguity reduction
  - collaborator strength is mechanization (Coq/Agda/Redex/Isabelle)
- Deliverable:
  - one mechanized nontrivial law fragment (usually right identity or assoc case split)

## Recommended Fit For This Repo (Current)

- Primary: `1) Bisimulation-first`
- Secondary: `3) Translation/correspondence`
- Escalation path: use `2) Step-indexed` if coinductive closure repeatedly stalls.

## Collaboration Split (Practical)

- Semantics lead:
  - define `≈`, theorem statements, and proof obligations
- Implementation lead:
  - maintain executable regression/law probes aligned with assumptions
- Formalization lead:
  - mechanize one core lemma early
- Writing lead:
  - maintain claim ledger (`proved`/`assumed`/`tested-only`) in paper text

## Immediate Next Actions

- [ ] Rewrite theorem statements in `monad-laws.rkt` using chosen `≈`.
- [ ] Convert `third-monad-law.rkt` from raw derivation transcript to a structured lemma plan.
- [ ] Add a one-page "Known limits of equivalence" section to the draft.
