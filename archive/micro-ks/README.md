# micro-ks Branch (`a9-skmonads2`)

## Development

This branch tries to re-derive 3k behavior from a small micro-language
starting point (`micro-ks.rkt` through `micro-ks-5.rkt`) rather than
from the larger map/join branch.

## Aim

- isolate continuation threading questions in a minimal language
- focus on `take`/`looper` and fuel-like stepping behavior
- understand where `dk` should be introduced and propagated

## Context

- narrative pointer:
  `/Users/jhemann/Documents/mk-search-w-continuations/old-mk-cps/research-story.txt`
  (see the section describing confusion around `looper` and pivot into
  `micro-ks-X` files)

This branch is highly relevant to future work on explicit stepping and
state-machine-oriented control flow.

## Status

`historical-reentry-candidate`:
- not active in current derivation gate
- one known broken variant (`micro-ks-3.rkt`)
- high value if/when resuming the state-machine/backtracking route
