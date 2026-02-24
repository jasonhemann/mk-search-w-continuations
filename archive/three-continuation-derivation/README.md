# Three-Continuation Derivation (formerly `map-join-with-dan`)

## Development

This branch is an iterative sequence (`map-join-with-dan.rkt` through
`-10`) plus `map-join-3-k.rkt`. It explores multiple formulations of
the same search machinery while changing continuation arities and order.

## Version Notes

These notes were absorbed from the former changes log, so this
directory now holds the primary timeline:

1. `map-join-with-dan.rkt`: original sk/fk behavior; clearly not yet at the desired model.
2. `map-join-with-dan-2.rkt`: first insertion of `dk`, but not in settled positions.
3. `map-join-with-dan-3.rkt`: initial nat-stream focus before the broader stream derivation.
4. `map-join-with-dan-4.rkt`: interleaving behavior study via a small fixed recursive cycle.
5. `map-join-with-dan-5.rkt`: shift where `sk` no longer takes `dk` in the `map` lambda path; `bind` expressed via `map`/`join`.
6. `map-join-with-dan-6.rkt`: moved `dk` to final argument position; later assessed as likely the wrong direction.
7. `map-join-with-dan-7.rkt`: attempted direct `bind` plus round-tripping to `map`/`join`; contains partially successful forms.
8. `map-join-with-dan-7b.rkt`: moved `fk` after `dk`; introduced "star" operator; includes notable alternative commented `bind` forms.
9. `map-join-with-dan-8.rkt`: backed off star operator; has an sk/fk-focused `bind` variant that appears to lose `dk` context.
10. `map-join-with-dan-8b.rkt`: compares with `7b`; argument order changes toward `dk`-first with partial operator coverage.
11. `map-join-with-dan-8c.rkt`: mostly formatting/printing cleanup.
12. `map-join-with-dan-9.rkt`: removes the newer star operation from `8c`.
13. `map-join-with-dan-10.rkt`: uncurried presentation to improve readability.

Supplement:
- `map-join-3-k.rkt`: compact 3k-focused branch used to probe continuation interactions.

## Aim

- derive a workable 3-continuation model (`sk/fk/dk`) from simpler
  stream-like and 2k behavior
- keep `bind` and `map/join` stories aligned
- understand `run`/`looper` seeding and fuel behavior

## Context

- broader rationale:
  `/Users/jhemann/Documents/mk-search-w-continuations/old-mk-cps/research-story.txt`

This line is the closest precursor to the later derivation framing in
`new-derivation/`.

## Status

`historical-keystone`:
- important for reconstructing design decisions
- not an active regression gate
- contains probe-heavy top-level forms and exploratory churn
