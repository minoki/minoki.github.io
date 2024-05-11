---
title: "Playing with Visible Forall in GHC 9.10"
---

*Japanese version*: [GHC 9.10で実装された可視なforallで遊ぶ](https://zenn.dev/mod_poppo/articles/playing-with-visible-forall)

Today, GHC 9.10.1 is released.
One of the new features is “visible forall”, or `RequiredTypeArguments` extension.
In this article, I will play with this new feature.

The official documents of this feature are these:

* [ghc-proposals/proposals/0281-visible-forall.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst)
* [6.4.18. Required type arguments — Glasgow Haskell Compiler 9.10.1 User's Guide](https://downloads.haskell.org/ghc/9.10.1/docs/users_guide/exts/required_type_arguments.html)

## Basics: The identity function

The simplest example would be the variant of `id` function that takes the type explicitly.
The ordinary `id` function and the variant of `id` function with visible `forall` can each be written as follows:

```haskell
{-# LANGUAGE RequiredTypeArguments #-}

-- From User's Guide:

-- The ordinary id function
id :: forall a. a -> a
id x = x

-- The id function with visible forall
id_vdq :: forall a -> a -> a
id_vdq a x = x
```

Let's try them with GHCi:

```
ghci> :set +t
ghci> id 42  -- Let GHC infer the type
42
it :: Num a => a
ghci> id @Int 42  -- Supply the type explicitly
42
it :: Int
ghci> id_vdq _ 42  -- Let GHC infer the type
42
it :: Num w => w
ghci> id_vdq Int 42  -- Supply the type explicitly (note that there's no `@`!)
42
it :: Int
```

So, by declaraing a function with `forall ->`, you can pass the type without using `@`.

Note that if the same notation is used for a term and a type, the term interpretation takes precedence.
Let's pass the `[Int]` type:

```
ghci> id_vdq [Int] [42]
<interactive>:37:8: error: [GHC-83865]
    • Expected a type, but ‘[Int]’ has kind ‘[*]’
    • In the type ‘[Int]’
      In the expression: id_vdq [Int] [42]
      In an equation for ‘it’: it = id_vdq [Int] [42]
```

An error occurs because `[Int]` is interpreted as “a type-level list with `Int` as an element” instead of “the type of a list consisting of `Int`”.
There are two ways around this problem.

The first is to use a `type` expression, which will be available with the `ExplicitNamespaces` extension.

```
ghci> :set -XExplicitNamespaces 
ghci> id_vdq (type [Int]) [42]
[42]
it :: [Int]
```

The other is to stop using the same notation for terms and types.
For list and tuple types, the `Prelude.Experimental` module provides aliases such as `List` and `Tuple2`.

```
ghci> :m + Prelude.Experimental
ghci> id_vdq (List Int) [42]
[42]
it :: [Int]
```

## Binary operator

Binary operators can take a type.
Let's consider the following function:

```haskell
{-# LANGUAGE RequiredTypeArguments #-}

as :: forall a. a -> forall a' -> a ~ a' => a'
as x _ = x
```

This function serves like a type annotation, when used as an infix operator:

```
ghci> :set +t
ghci> 42 `as` Integer
42
it :: Integer
ghci> 42 `as` Rational
42 % 1
it :: Rational
ghci> 42 `as` Double
42.0
it :: Double
```

This is not much to be thankful for, but you can easily create function that specify “part of a type”:

```haskell
{-# LANGUAGE RequiredTypeArguments #-}

as' :: forall f a. f a -> forall a' -> a ~ a' => f a'
as' x _ = x
```

```
ghci> :m + Data.Functor.Identity
ghci> Identity 42 `as'` Int
Identity 42
it :: Identity Int
ghci> Identity 42 `as'` Rational
Identity (42 % 1)
it :: Identity Rational
```

Of course, specifying part of a type has been possible with `PartialTypeSignatures` extension.

## Type classes

It would be useful to be able to receive the type in a typeclass method.
For example, it would be simpler to write `sizeOf Int` instead of `sizeOf (undefined :: Int)`.
Is such a definition possible?

```haskell
-- Hypothetical code
class NewStorable a where
  sizeOf :: forall a -> Int
```

Unfortunately, this will not work.
The `a` in typeclass and the `a` in `forall` are different variables.

```haskell
-- Actual interpretation
class NewStorable a where
  sizeOf :: forall a' -> Int

-- Externally-visible type would be:
-- sizeOf :: forall a. NewStorable a => forall a' -> Int
```

The right way is to create a wrapper.

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}

class NewStorable a where
  sizeOf_ :: Int

-- A wrapper
sizeOf :: forall a -> NewStorable a => Int
sizeOf a = sizeOf_ @a
```

An alternative way is a trick using `~`:

```haskell
class NewStorable a where
  sizeOf :: forall a' -> a ~ a' => Int
```

However, using an extra `=>` may cause subtle differences in intermediate code.
Consider the following code:

```haskell
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
import Debug.Trace
import Data.Proxy

newtype Tagged t a = MkTagged { unTagged :: a }

class Foo a where
  someValueAmb :: Int
  someValueTagged :: Tagged a Int
  someValueProxy :: Proxy a -> Int
  someValueVis :: forall a' -> a ~ a' => Int

instance Foo Float where
  someValueAmb = trace "some heavy computation 1" 42
  someValueTagged = MkTagged (trace "some heavy computation 2" 42)
  someValueProxy _ = trace "some heavy computation 3" 42
  someValueVis _ = trace "some heavy computation 4" 42

main :: IO ()
main = do
  print (someValueAmb @Float)
  print (someValueAmb @Float)
  print (unTagged (someValueTagged :: Tagged Float Int))
  print (unTagged (someValueTagged :: Tagged Float Int))
  print (someValueProxy (Proxy @Float))
  print (someValueProxy (Proxy @Float))
  print (someValueVis Float)
  print (someValueVis Float)
```

Suppose `someValue` is a heavy computation.
In this code, I use `trace` instead of actual computation.

In `main`, each `someValue` is called twice.
How many times will the right side of `someValue` be evaluated?

If optimizations are enabled, each `someValue` is evaluated once.

```
$ ghc-9.10 -O1 Test.hs
$ ./Test
some heavy computation 1
42
42
some heavy computation 2
42
42
some heavy computation 3
42
42
some heavy computation 4
42
42
```

But what happen if optimizations are disabled?

```
$ ghc-9.10 -O0 Test.hs
$ ./Test
some heavy computation 1
42
42
some heavy computation 2
42
42
some heavy computation 3
42
some heavy computation 3
42
some heavy computation 4
42
some heavy computation 4
42
```

`someValueAmb` and `someValueTagged` were evaluated only once each, whereas `someValueProxy` and `someValueVis` were evaluated twice each.
This reflects the difference whether these entities are the value `Int` or the function `_ -> Int`.

In a simple program like this, there would be no difference if optimizations are enabled.
But in a more complex and intricate program, the optimizations may not work well enough.
If efficiency is the most important thing to you, you should keep this in mind.

## Theorem proving

Type-level programming is common in Haskell.
For example, the type-level list concatenation `++` can be defined as follows:

```haskell
type (++) :: [k] -> [k] -> [k]
type family (++) xs ys
type instance '[] ++ ys = ys
type instance (x ': xs) ++ ys = x ': (xs ++ ys)
```

The associativity law `xs ++ (ys ++ zs) = (xs ++ ys) ++ zs` holds on list concatenation, but GHC's type checker doesn't know that.
To teach the type checker an non-trivial equality, we use theorem proving.
That is, we define a function like this:

```haskell
import Data.Type.Equality ((:~:))

appendIsAssociative :: ... -> xs ++ (ys ++ zs) :~: (xs ++ ys) ++ zs
```

To prove the associativity, we can use structural induction on `xs`.
That is, if `xs = '[]` then it is obvious.
If `xs = x : xss`, then we can prove the equation with:

```
(x : xss) ++ (ys ++ zs)
  = x : (xss ++ (ys ++ zs))  (by definition of ++)
  = x : ((xss ++ ys) ++ zs)  (by induction hypothesis)
  = (x : (xss ++ ys)) ++ zs  (by definition of ++)
  = ((x : xss) ++ ys) ++ zs  (by definition of ++)
```

Now, let's implement it.
Since we want to prove by case on `xs`, we define a data type to enable pattern-matching:

```haskell
data ProxyList xs where
  PNil :: ProxyList '[]
  PCons :: Proxy x -> ProxyList xs -> ProxyList (x ': xs)
```

On the other hand, we don't need to do case analysis on `ys` and `zs`, the parameter can be something like `Proxy`.
Therefore, our “proof” will be a function with the following signature:

```haskell
appendIsAssociative :: ProxyList xs -> proxy2 ys -> proxy3 zs -> xs ++ (ys ++ zs) :~: (xs ++ ys) ++ zs
```

The body of the proof, written according to the equational transformation, is as follows:

```haskell
appendIsAssociative PNil _ _ = Refl
appendIsAssociative (PCons (_ :: _ x) (xss :: _ xss)) (ys :: _ ys) (zs :: _ zs) = let
    pf1 :: (x : xss) ++ (ys ++ zs) :~: x : (xss ++ (ys ++ zs))
    pf1 = Refl
    pf2 :: x : (xss ++ (ys ++ zs)) :~: x : ((xss ++ ys) ++ zs)
    pf2 = apply Refl (appendIsAssociative xss ys zs)
    pf3 :: x : ((xss ++ ys) ++ zs) :~: (x : (xss ++ ys)) ++ zs
    pf3 = Refl
    pf4 :: (x : (xss ++ ys)) ++ zs :~: ((x : xss) ++ ys) ++ zs
    pf4 = Refl
  in pf1 `trans` pf2 `trans` pf3 `trans` pf4
```

Now we have the proof, but the above is not very ideal.
We wrote the intermediate types twice each.
Is there any way to simplify the code while maintaining the readability of the proof?

Ideally, I would like to make the equation

```
(x : xss) ++ (ys ++ zs)
  = x : (xss ++ (ys ++ zs))  (by definition of ++)
  = x : ((xss ++ ys) ++ zs)  (by induction hypothesis)
  = (x : (xss ++ ys)) ++ zs  (by definition of ++)
  = ((x : xss) ++ ys) ++ zs  (by definition of ++)
```

a valid Haskell code.

Before GHC 9.10, we might have used singleton types for this kind of thing.
But now we have `RequiredTypeArguments`.
Let's use `RequiredTypeArguments` to achieve this notation.

The basic idea is to define operators `===` and `by` to have the following expression the type `a :~: c`:

```
〈proof of a = b〉 === c `by` 〈proof of b = c〉
```

We use `forall ->` in `===` to receive the type `c`.
That is, the operators `===` and `by` should have the types

```haskell
(===) :: a :~: b -> forall c -> ???
by :: ??? -> b :~: c -> a :~: c
```

The `???` part must contain the information for `a`, `b`, `c`.
Here, I use `(a :~: b, Proxy c)`.

In summary, the proof can now be written as follows:

```haskell
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
import Data.Type.Equality
import Data.Proxy
import Prelude hiding ((++))

type (++) :: [k] -> [k] -> [k]
type family (++) xs ys
type instance '[] ++ ys = ys
type instance (x ': xs) ++ ys = x ': (xs ++ ys)

infixl 1 ===, `by`

(===) :: a :~: b -> forall c -> (a :~: b, Proxy c)
(===) x _ = (x, Proxy)

by :: (a :~: b, Proxy c) -> b :~: c -> a :~: c
by (Refl, _) Refl = Refl

beginProof :: forall a -> a :~: a
beginProof _ = Refl

appendIsAssociative :: ProxyList xs -> proxy2 ys -> proxy3 zs -> xs ++ (ys ++ zs) :~: (xs ++ ys) ++ zs
appendIsAssociative PNil _ _ = Refl
appendIsAssociative (PCons (_ :: _ x) (xss_ :: _ xss)) (ys_ :: _ ys) (zs_ :: _ zs)
  = beginProof ((x : xss) ++ (ys ++ zs))
           === x : (xss ++ (ys ++ zs)) `by` Refl
           === x : ((xss ++ ys) ++ zs) `by` apply Refl (appendIsAssociative xss_ ys_ zs_)
           === (x : (xss ++ ys)) ++ zs `by` Refl
           === ((x : xss) ++ ys) ++ zs `by` Refl

data ProxyList xs where ...
```

There are a couple of remarks:

* Using type-level `++` as an argument to `forall ->` is ambiguous with term-level `++`. You could use `type`, but it would be too noisy, so here we hide the `++` from `Prelude`.
* In the previous code, I used the same variable names at the term level and the type level, like `xss :: _ xss`, but this is also inconvenient, so I changed the variable names at the term level.

Now the proof is cool enough, but can it be improved further?
For example, can we just use `appendIsAssociative ...` without using `apply Refl` when applying induction hypothesis?

Yes.
The implementation is as follows:

```haskell
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RequiredTypeArguments #-}
import Data.Type.Equality
import Data.Proxy
import Prelude hiding ((++))

type (++) :: [k] -> [k] -> [k]
type family (++) xs ys
type instance '[] ++ ys = ys
type instance (x ': xs) ++ ys = x ': (xs ++ ys)

infixl 1 ===, `by`

(===) :: a :~: b -> forall c -> b ~ c => a :~: c
(===) Refl _ = Refl

by :: (s ~ t => prop) -> s :~: t -> prop
by proof Refl = proof

beginProof :: forall a -> a :~: a
beginProof _ = Refl

appendIsAssociative :: forall xs -> ProxyListI xs => forall ys -> forall zs -> xs ++ (ys ++ zs) :~: (xs ++ ys) ++ zs
appendIsAssociative xs ys zs = case proxyList' @xs of
  PNil' -> Refl
  PCons' @x @xss ->
    beginProof ((x : xss) ++ (ys ++ zs))
           === x : (xss ++ (ys ++ zs))
           === x : ((xss ++ ys) ++ zs) `by` appendIsAssociative xss ys zs
           === (x : (xss ++ ys)) ++ zs
           === ((x : xss) ++ ys) ++ zs

data ProxyList' xs where
  PNil' :: ProxyList' '[]
  PCons' :: forall x xs. ProxyListI xs => ProxyList' (x ': xs)

class ProxyListI xs where
  proxyList' :: ProxyList' xs

instance ProxyListI '[] where
  proxyList' = PNil'

instance ProxyListI xs => ProxyListI (x ': xs) where
  proxyList' = PCons' @x @xs
```

## Acknowledgements

I must thank all the people who worked for GHC 9.10 release, especially Serokell's GHC team for the work on dependent types.
Thank you!
