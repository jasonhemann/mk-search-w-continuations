# mk-search-w-continuations

## Continuation-based implementation of miniKanren search

## How to visit:

-  First read this README documentation.
-  Then, read `./old-mk-cps/research-story.txt`
-  Then, read `Church-encoding-lists-and-streams.org`
-  Then, follow `./new-derivation/mk-streams-derivation-1.rkt` all the way through `-4`.
-  Simultaneously, read `./new-derivation/continuation-derivation-notes.org`

## A continuation-based implementation of the miniKanren search.

This work is directly preceded in parts by Danvy et al.'s "A Unifying
Approach to Goal-Directed Evaluation". They implement a backtracking
language via a deep embeddings in monadic interpreters. They implement
multiple versions including sk/fk based, and also stream based
monads. They also compare and relate these implementations. 

Unlike Danvy et al., lazy, odd-initial ('odd' in the terminology of
Wadler's "... Without Even Being Odd") streams themselves are possible
returned values. This change impacts both the datatype that we can
possibly return, as well as the structure of the continuations.

```
 /k (one continuation) <-----> lazy list 
  ^                            ^ 
  |                            |
  |                            |
  |                            |
  |                            |
  |                            |
  |                            |
  v                            v
 sk/fk <---------------> backtracking monad
  ^                            ^ 
  |                            |
  |                            |
  |                            |
  |                            |
  |                            |
  |                            |
  v                            v
sk/fk/dk          heuristic interleaving backtracking monad(!)
```

We have an odd-looking, specialized version of the basic stream monads
implicit in our microKanren style search. We have reason to expect
that our heuristic interleaving backtracking stream-based search has
an underlying monad, and we want to find the continuation-based
analogue. If we want to find this continuation-based monadic version,
one thing we could do is try and build a monadic interpreter for a
microKanren-like language, for what we expect the monad should look
like, for the stream based implementation. This gives a point of
comparison w/Danvy et al. We expect that our version should bear a
strong resemblance. Further, we understand the translation at Danvy et
al.'s level between the streams and continuation monads. This should
mean that we have two directions from which to approach the sk/fk/dk
model: down, and to the left, as expressed in the diagram above. This
might help to shine a light on the parts that we don't yet know how to
translate.

We will use Church encoding of the datatypes as did Danvy (or Wand),
but try and carry that forward one more level by just continuing the
analogy/pattern.

For yet another point of comparison and to get more intuition, we will
also use the two equivalent monad definitions (the map-join, and the
unit-bind) and translate between them. This both gives additional
intuition in "feeling our way through", and also provides a way to
check some of our work along the way.

We specifically relied on Dave Herman's detailed translations in a
Schemely syntax.

Wand and Vallaincourt extended and revised Danvy et al, and their
revisiting and extending makes it a bit easier still to follow some of
the machinations in Danvy et al.

We anticipate semantics directed compilation by partial evaluation. 

# Directory Structure 

I structured the contents of this directory as follows:

