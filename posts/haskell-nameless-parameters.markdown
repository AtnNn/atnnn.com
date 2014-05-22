---
title: Writing Haskell Functions With Many Nameless Parameters
date: 2011-10-03
category: code
---

Although it could be considered bad style, it is often useful to have
functions that take a large amount of similar parameters that vary in
type. I am going to demonstrate a technique that allows to write such
functions without naming these parameters and making the function more
readable.

The function I will use for demonstration is an example of a common
pattern when working with JSON data.

```haskell
{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Types
import Data.Text

person ∷ String → Float → Bool → Value
person name height over18 = object [
  "name" .= name,
  "height" .= height,
  "over18" .= over18]
```

The name of each parameter is repeated three times. Writing many such
functions is often done by copying and pasting, which is a red flag
and a possible source of errors.

In an attempt to write reusable  code, we can define a simple function
that builds the result in small steps.

```haskell
field ∷ ToJSON a ⇒ Text → [Pair] → a → [Pair]
field k d v =  (k, toJSON v) : d
```

The order of the arguments is important because it fits well into the
`$-` combinator. This combinator is the key to the whole exercise. It
builds our desired multi-argument function without having to name the
arguments.

```haskell
infixr 3 $-
($-) ∷ (d → a → e) → (e → k) → d → a → k
($-) f g d a = g $ f d a
```

(Some readers may recognise this relative of the infamous cartoon face operator `((.).(.))`)

We can now rewrite the person function.

```haskell
person' ∷ String → Float → Bool → Value
person' = ($[])
  $  field "name"
  $- field "height"
  $- field "over18"
  $- object
```

The first part of the function body, `($[])`, is the initial value
used to build our object. The last part, `$- object` transforms the
list that was built incrementally into the final result.

This technique becomes more powerful when you write other helper
functions. For example, we can also have another function that ignores
`Nothing`.

```haskell
optfield ∷ ToJSON a ⇒ Text → [Pair] → Maybe a → [Pair]
optfield k d (Just v) = field k d v
optfield _ d Nothing = d
```

The `$-` combinator's usefulness is not limited to building JSON
objects. It can be used to write many functions that need to combine
heterogeneous values for which simple utility functions such as field
and optfield can be written. Functions written in this style make
cleaner, less repetitive code.
