---
title: Guessing the implementation of combinators
date: 2012-05-16
category: code
---

I completed Oleg's de-typechecker and put it on hackage as
`Guess.Combinator`.

<http://hackage.haskell.org/packages/archive/guess-combinator/0.1.1/doc/html/Guess-Combinator.html>

The original article had left part of the code as an exercise for the
reader. Completing it on your own would be great practice for learning
type-level programming.

<http://www.haskell.org/pipermail/haskell/2005-March/015423.html>

How is this useful? Take the `$-` operator for example.

<http://www.atnnn.com/p/haskell-nameless-parameters/>

It can now be written as

``` haskell
($-) ∷ (d → a → e) → (e → k) → d → a → k
($-) = combinator
```

Haskell is magic!﻿