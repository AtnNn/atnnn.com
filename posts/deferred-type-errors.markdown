---
title: Deferred type errors in Haskell
date: 2012-05-13
category: code
---

Deferred type errors will be in GHC 7.6!

<http://hackage.haskell.org/trac/ghc/wiki/Status/May12>

I've wanted this for a long time. I first tried implementing it in 2010 as a
perl script. I completed my first working implementation in 2011.

<http://hackage.haskell.org/trac/ghc/ticket/5624#comment:1>

The idea was not new.

<https://plus.google.com/107890464054636586545/posts/EbSuoRA6FTw>

and Simon Peyton Jones had already thought about a better way to implement it.

<http://hackage.haskell.org/trac/ghc/ticket/5624#comment:4>

The idea is described in much more detail in the paper "Equality proofs and
deferred type errors".

<http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/icfp12.pdf>

I have been working with deferred type errors for the past 6 months. One of the
benefits is to be able to interact with code in ghci even when there is a type
error. Especially for large unknown codebases such as GHC.

Deferred errors replace the sometimes tedious step of commenting out code and
replacing it with undefined, and later removing undefined and uncommenting.

For example, I can `:load TcInteract.lhs` to test my modifications. If I
introduced a type error, it shows as a warning and I can immediately use
`:type`, `:info`, and `:browse` on identifiers from other modules and from
`TcInteract` as well.

Deferred type errors are also useful when refactoring code. When I change the
type of a function that is used in many other files, GHCi can list, in a single
run, all the type errors across all files. I can then easily fix all the use
sites of that function by skipping from one error to the next in my IDE.
Previously, the process was iterative because GHC stopped after each file
containing an error.
