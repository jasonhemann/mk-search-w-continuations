# map-join-with-dan Branch

## Development

This branch is an iterative sequence (`map-join-with-dan.rkt` through
`-10`) plus `map-join-3-k.rkt`. It explores multiple formulations of
the same search machinery while changing continuation arities and order.

## Aim

- derive a workable 3-continuation model (`sk/fk/dk`) from simpler
  stream-like and 2k behavior
- keep `bind` and `map/join` stories aligned
- understand `run`/`looper` seeding and fuel behavior

## Context

- narrative timeline:
  `/Users/jhemann/Documents/mk-search-w-continuations/old-mk-cps/changes-in-m-j-w-d-versions.txt`
- broader rationale:
  `/Users/jhemann/Documents/mk-search-w-continuations/old-mk-cps/research-story.txt`

This line is the closest precursor to the later derivation framing in
`new-derivation/`.

## Status

`historical-keystone`:
- important for reconstructing design decisions
- not an active regression gate
- contains probe-heavy top-level forms and exploratory churn
