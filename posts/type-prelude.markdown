---
title: type-prelude and GHC 7.6.1
date: 2012-09-06
category: code
---

The [new release of
GHC](http://www.haskell.org/ghc/docs/7.6.1/html/users_guide/release-7-6-1.html)
has many new features. [Deferred type
errors](https://plus.google.com/102696032232347740796/posts/bEiZmixWJgB)
are very useful but the other features are a lot more fun. It is now a
lot easier to encode invariants, information and computation at the
type level. How much of the prelude can be ported, and how difficult
is it to port?

All the code from this article is available in the [type-prelude
package on hackage](http://hackage.haskell.org/package/type-prelude).

## Integer

The prelude has an Integer type that can represent any positive or
negative integer. GHC has a new feature, data kind promotion, which
automatically promotes data types and constructors into data kinds and
types when it is possible. It is not possible to promote this type
because its implementation is hidden.

GHC has a Nat kind which allows any natural number to be used as a
type, but they cannot be used to replace Integer.

The `Prelude.Type.Integer` module provides the `Integer` kind, implemented
as an unbounded list of bits using twos-complement. The `I` class
converts small `Nat`s into Integers.

``` haskell
data Integer = One Integer | Zero Integer | Ones | Zeros

type family I (a :: Nat) :: Integer
type instance I 0 = Zeros
type instance I 1 = One Zeros
type instance I 2 = Zero (One (Zeros))
type instance I 3 = One (One Zeros)
type instance I 4 = Zero (Zero (One Zeros))
```

Three extra type functions allow working with `Integer`s as if they really were lists of `Bool`s.

```haskell
type family IntegerHead (i :: Integer) :: Bool
type family IntegerTail (i :: Integer) :: Integer
type family IntegerCons (b :: Bool) (i :: Integer) :: Integer
```

Most of the functions and operators from `Num` and `Integral` can be
implemented very easily for this `Integer` kind. For example:

```haskell
type family Negate (a :: k) :: k
type instance Negate a = Complement a + I 1

type family   (a :: k) * (b :: k) :: k
type instance Zeros    * j = Zeros
type instance Ones     * j = Negate j
type instance (Zero i) * j = IntegerCons False (i * j)
type instance (One i)  * j = j + IntegerCons False (i * j)
```

## Proxy

Constructors, when promoted, becomes types. But these types do not
have any values. The `Prelude.Type.Value` provides the `T` type (known
elsewhere as `Proxy`).

```haskell
data T a = T
```

It also provides a method of converting a `T` into the value of its associated type.

```haskell
class Value (a :: k) (t :: *) | k -> t where value :: T a -> t
```

The `Value` instances for Integers converts each constructor into its corresponding value.

```haskell
instance Value Zeros Prelude.Integer where value _ = 0
instance Value Ones Prelude.Integer where value _ = -1
instance (Value i Prelude.Integer) => Value (One i) Prelude.Integer
    where value _ = shift (value (T::T i)) 1 Prelude.+ 1
instance (Value i Prelude.Integer) => Value (Zero i) Prelude.Integer
    where value _ = shift (value (T::T i)) 1
```

The `T::T` idiom is very common in code that uses `Prelude.Type`. It is made even more useful when using the `Show` instance for `T`.

```haskell
instance (Value a t, Prelude.Show t) => Prelude.Show (T a) where
   show _ = Prelude.show (value (T::T a))
```

It allows easy type-level programming in ghci.

```haskell
Prelude.Type> T::T (One (Zero Ones))
-3
```

## Lists

The list type has two well known constructors which GHC promotes into
types. To avoid confusion, the cons and nil types need to be prefixed
with a single quote.

Converting list functions into type-level functions is often very
straightforward. For example, the last function can be written like
this.

```haskell
type family Last (a :: [k]) :: k
type instance Last '[] = Error "Last_: empty list"
type instance Last (x ': y ': xs) = Last (y ': xs)
type instance Last (x ': '[]) = x
```

And it works:

```haskell
Prelude.Type.Families> T::T (Last [1,2,3])
3
```

## Type Families vs Type Classes

The `Last` function can also be implemented as a type class.

``` haskell
class Last (l :: [a]) (x :: a) | l -> x
instance Error "Last: empty list" => Last '[] a
instance a ~ b => Last '[a] b
instance Last (y ': xs) a => Last (x ': y ': xs) a
```

But classes need to be used differently.

```haskell
Prelude.Type> T :: Last [1,2,3] a => T a
3
```

Thanks to Constraint kinds, we can add a Value instance for simple constraints.

```haskell
instance (c a, Value a t) => Value (c :: k -> Constraint) t
  where value _ = value (T::T a)
```

Now we can use the `Last` class in the exact same way we used the `Last` type family.

```haskell
Prelude.Type> T::T (Last [1,2,3])
3
```

Code written using type families often looks nicer, so why use type
classes and functional dependencies? Because we can not (yet?)
partially apply type families. However type classes do not mind being
partially applied. We can easily write higher order type classes such
as `FoldR`.

```haskell
class FoldR (f :: a -> b -> b -> Constraint)
    (nil :: b) (list :: [a]) (ret :: b) | f nil list -> ret
instance Id nil ret => FoldR f nil '[] ret
instance (FoldR f nil xs tail, f x tail ret)
    => FoldR f nil (x ': xs) ret
```

`FoldR` can be used to implement `Or`.

```haskell
type Or = FoldR (||) '[]
```

`Or` can be used to implement `Any`.

```haskell
type Any = (Compose1 Or) `Compose2` Partial1 Map
```

Partial application of type classes does not work exactly the same way
as in regular Haskell, which is why the `Compose` class (corresponding
to (.)) has many variants. Each variant has a different arity. But it
still works.

```haskell
Prelude.Type> T::T (Any ((==) 1) '[3,2,1])
True
```

## If and Case

To make type-level programming easier, `Prelude.Type` also provides `If`
and `Case` classes. For example, `Case` is used to implement `Ord`.

```haskell
class Compare (a :: k) (b :: k) (o :: Ordering) | a b -> o

class ((a :: k) < (b :: k)) (p :: Bool) | a b -> p
instance (Compare a b o,
          Case o [LT --> True ~ p,
                  Otherwise (False ~ p)])
         => (a < b) p
```

And If is used to implement `Enum` for `Integer`s.

```haskell
instance If (b < a) (l ~ '[])
            (((I 1) + a) c, EnumFromTo c b k, l ~ (a ': k))
         => EnumFromTo (a :: Integer) b l
```

## Extras

`Prelude.Type` contains a port of most of the functions from the
Prelude that operate on `Bool`, `Integer`, lists and functions.

For fun, it also provides the `Main` type alias for writing executables entirely on the type level.

```haskell
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts, UndecidableInstances, KindSignatures #-}

module Main (main) where

import Prelude.Type

instance Gcd (I 12) (I 10) a
        => Main a

$ ghc Main.hs
$ ./Main.hs
2
```