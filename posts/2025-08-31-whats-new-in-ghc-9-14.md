---
title: An Unofficial Guide to What's New in GHC 9.14
---

I've been writing posts in Japanese over the past few years introducing new features in GHC.
I thought such posts might also be useful for readers in the English-speaking community, so with some help from tools like ChatGPT, I've translated the latest one into English.
I hope this will be helpful when you try out the new GHC.

The original Japanese article can be found at [GHC 9.14の新機能](https://zenn.dev/mod_poppo/articles/whats-new-in-ghc-9-14).

---

GHC 9.14.1-alpha1 was released on August 20, 2025 (in the author's timezone).

* [GHC 9.14.1-alpha1 released - Announcements - Haskell Community](https://discourse.haskell.org/t/ghc-9-14-1-alpha1-released/12786)

In this post, I’ll go through the new features in GHC 9.14, based on my own (somewhat subjective) perspective.

This article is not a comprehensive overview.
In particular, areas I’m less familiar with—like the RTS and Template Haskell—aren’t really covered.
Please also check out the official release notes:

* [2.1. Version 9.14.1 — Glasgow Haskell Compiler 9.14.0.20250819 User's Guide](https://downloads.haskell.org/ghc/9.14.0.20250819/docs/users_guide/9.14.1-notes.html)
    * [docs/users_guide/9.14.1-notes.rst · ghc-9.14 · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.14/docs/users_guide/9.14.1-notes.rst)
<!-- * [Changelog for base-4.22.0.0 | Hackage](https://hackage.haskell.org/package/base-4.22.0.0/changelog) -->
* [libraries/base/changelog.md · ghc-9.14 · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.14/libraries/base/changelog.md)
* [9.14 · Wiki · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.14)

If you want to try out the alpha release with GHCup, you’ll need to add the `prereleases` channel.
See [Release channels - User Guide - GHCup](https://www.haskell.org/ghcup/guide/#release-channels) for details.

```
$ ghcup config add-release-channel prereleases
$ ghcup install ghc 9.14.0.20250819
```

The reason I’m publishing this “what’s new” post while GHC is still in alpha is to encourage more people to give the alpha a try.
That way, we can catch as many issues as possible before the final release.
So, please do try it out!

# Long-Term Support (LTS)

* [GHC LTS Releases — The Glasgow Haskell Compiler](https://www.haskell.org/ghc/blog/20250702-ghc-release-schedules.html)
* Background
    * [#26067: Please revise the release policy · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/26067)
    * [Please revise GHC release policy - Links - Haskell Community](https://discourse.haskell.org/t/please-revise-ghc-release-policy/12158)

Until now, GHC has followed a schedule of releasing a new major version every six months.
Bug fixes and other patches would typically be backported to around the three most recent branches (currently GHC 9.10, 9.12, and 9.14 — [see status](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC-status)).

However, it usually takes time for a given major version to become stable through bug fixes, and for the ecosystem to catch up.
By the time a release feels “stable enough to use,” its support window is often already approaching its end.
For example, as of August 2025, GHCup recommends 9.6.7, but the 9.6 series itself is already out of support.

To address this, GHC will now designate some major versions as Long-Term Support (LTS) releases, aimed at users who want a stable version they can rely on for a longer period.
LTS releases will receive support for about 2–3 years. The first LTS release will be GHC 9.14.

# New Features in GHC 9.14

## SPECIALIZE pragmas can now take expressions

* [Allow expressions in SPECIALISE pragmas - ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0493-specialise-expressions.rst)

In GHC, parametric polymorphism and type classes are implemented using type erasure and dictionary passing, which generally come with some runtime cost.
To generate efficient code, optimizations such as **inlining** and **specialization** are used.

To guide the compiler on inlining and specialization, we can use the `INLINE` pragma and the `SPECIALIZE` pragma.

Traditionally, the syntax for `SPECIALIZE` was:

```haskell
{-# SPECIALIZE <name> :: <type> #-}
```

For example, if you had `someFunc :: Monad m => m a -> m b -> m ([a], b)` and wanted to specialize it for `m = StateT Int IO`, you had to write:

```haskell
{-# SPECIALIZE someFunc :: StateT Int IO a -> State Int IO b -> StateT Int IO ([a], b) #-}
```

This gets cumbersome when the type you want to specialize appears multiple times—you have to repeat it everywhere.

Now, `SPECIALIZE` pragmas can take expressions.
In particular, with the help of the `TypeApplications` extension, the example above can be written as:

```haskell
{-# SPECIALIZE someFunc @(StateT Int IO) #-}
```

You can also specialize with respect to argument values.
For example:

```haskell
foo :: Int -> [Int] -> [Int]
foo !a = map (+ a)
{-# INLINE [0] foo #-}
{-# SPECIALIZE foo 0 #-}
{- Equivalent to defining a rewrite rule: foo 0 = map (+ 0) -}
```

Like rewrite rules, you can also use variables with `forall`.
For example, if you want to specialize on the second argument of:

```haskell
pow :: Num a => a -> Int -> a
pow _ 0 = 1
pow x 1 = x
pow x n | even n    = pow (x * x) (n `quot` 2)
        | otherwise = x * pow (x * x) (n `quot` 2)
```

you can write:

```haskell
{-# SPECIALIZE forall x. pow x 1 #-}
{-# SPECIALIZE forall x. pow x 2 #-}
{-# SPECIALIZE forall x. pow x 3 #-}
{-# SPECIALIZE forall x. pow x 4 #-}
{-# SPECIALIZE forall x. pow x 5 #-}
```

As an undocumented feature, it used to be possible to write multiple types in a comma-separated list:

```haskell
{-# SPECIALIZE foo :: T1, T2 #-}
```

But this form is scheduled for removal in GHC 9.18.

## `-Wincomplete-record-selectors` is now enabled by `-Wall`

In GHC 9.10, the option `-Wincomplete-record-selectors` was introduced, which warns when a record selector that can fail is used. Starting with GHC 9.14, this option is now enabled as part of `-Wall`.

```haskell
data T = A { a :: Int }
       | B { b :: String }

f :: T -> String
f t = b t ++ "\n"
```

```
$ ghc-9.14 -Wall recordsel.hs
recordsel.hs:5:7: warning: [GHC-17335] [-Wincomplete-record-selectors]
    Selecting the record field ‘b’ may fail for the following constructors:
      A
  |
5 | f t = b t ++ "\n"
  |       ^
```

## Changes around ScopedTypeVariables, TypeApplications, and TypeAbstractions

Previously, when both the `ScopedTypeVariables` and `TypeApplications` extensions were enabled, it was possible to write type bindings directly in patterns. For example, the following code compiles in GHC 9.12:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

data T a = MkT a

f :: forall a. T a -> a
f t = case t of
    MkT @a2 x -> x
```

Starting with GHC 9.14, such code now requires the `TypeAbstractions` extension to be enabled.

## Changes in desugaring for OverloadedRecordUpdate

* [HasField redesign - ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0583-hasfield-redesign.rst)

In connection with Haskell’s record-related extensions, there is a class called `HasField`:

```haskell
module GHC.Records where
class HasField x r a | x r -> a where
  getField :: r -> a
```

Combined with the `DuplicateRecordFields` extension, this allows you to work with different records that share the same field name (via the `getField` function):

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
import GHC.Records

data S = S { foo :: Int }
data T = T { foo :: String }

main :: IO ()
main = do
  print $ getField @"foo" (S 42)
  putStrLn $ getField @"foo" (T "Hello")
```

Currently, the `HasField` class only supports retrieving record fields.
There is a plan to extend this so that fields can also be updated.

Originally, the idea was to add a `setField` function with the following type:

```haskell
setField :: forall x r a. HasField x r a => r -> a -> r
```

Indeed, in GHC 9.2, Record Dot Syntax was implemented to desugar into `setField` of this form.

Under the new plan, the argument order of `setField` changes to:

```haskell
class SetField x r a | x r -> a where
  ...
  setField :: a -> r -> r
```

In other words, the field value now comes first, followed by the record.

With this new argument order in mind, examples using `OverloadedRecordDot` would look like this:

```haskell
{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}
import Prelude
import GHC.Records

data Foo = Foo { x :: Int } deriving Show

data Bar = Bar { foo :: Foo
               , y :: String
               } deriving Show

class HasField' x r a | x r -> a where
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0)
  setField :: a -> r -> r
#else
  setField :: r -> a -> r
#endif

instance HasField' "x" Foo Int where
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0)
  setField x _ = Foo x
#else
  setField _ x = Foo x
#endif

instance HasField' "foo" Bar Foo where
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0)
  setField foo Bar { foo = _, y = y } = Bar { foo = foo, y = y }
#else
  setField Bar { foo = _, y = y } foo = Bar { foo = foo, y = y }
#endif

u = Bar { foo = Foo { x = 42 }, y = "Hello!" }

main = print (u { foo.x = 37 })
```

## Allowing MultilineStrings in `foreign import`

* [#25157: Support multiline strings in foreign import · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25157)

You can now use multiline string literals (via the `MultilineStrings` extension) in `foreign import` declarations.
This should be especially handy for JavaScript FFI.

Here’s an example from the issue:

```haskell
foreign import javascript
  """
  (() => {
    console.log("hello");
    console.log(1 + 1);
  })
  """
  action :: IO ()
```

## `coerce` and Type Defaulting

* [#21003: Coercible constraints should be picked arbitrarily · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/21003)

Ambiguous types involving the `coerce` function or the `Coercible` class are now resolved more aggressively.
For example, the following program would trigger a type ambiguity error in GHC 9.12, but it compiles successfully in GHC 9.14:

```haskell
import Data.Coerce

main :: IO ()
main = print $ coerce (42 :: Int)
```

```
$ runghc-9.12 coerce.hs
coerce.hs:4:16: error: [GHC-10283]
    • Couldn't match representation of type ‘a0’ with that of ‘Int’
        arising from a use of ‘coerce’
    • In the second argument of ‘($)’, namely ‘coerce (42 :: Int)’
      In the expression: print $ coerce (42 :: Int)
      In an equation for ‘main’: main = print $ coerce (42 :: Int)
  |
4 | main = print $ coerce (42 :: Int)
  |                ^^^^^^

$ runghc-9.14 coerce.hs
42
```

## Specifying Record Field Multiplicity under the `LinearTypes` Extension

* [ghc-proposals/proposals/0111-linear-types.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst#records-and-projections)
* [#18462: Linear types syntax: multiplicity annotation on records · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/18462)

With the `LinearTypes` extension, you can now specify the multiplicity of record fields.

```haskell
{-# LANGUAGE LinearTypes #-}
import GHC.Exts

data A = MkA Int

data B where
  MkB :: Int -> B

data C where
  MkC :: Int %'Many -> Int %'One -> C

-- New feature in GHC 9.14
data R = R { foo %'Many :: Int, bar %'One :: Int }

fA :: A %1-> Int
fA (MkA x) = x

fB :: B %1-> Int
fB (MkB x) = x

-- Error (y is discarded)
-- fC1 :: C %1-> Int
-- fC1 (MkC x y) = x

fC2 :: C %1-> Int
fC2 (MkC x y) = y

-- Error (bar is discarded)
-- fR1 :: R %1 -> Int
-- fR1 (R { foo, bar }) = foo

fR2 :: R %1 -> Int
fR2 (R { foo, bar }) = bar

main :: IO ()
main = pure ()
```

## You can now use `data` with the `ExplicitNamespaces` extension

* [Namespace-specified imports - ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0581-namespace-specified-imports.rst)

Haskell has separate namespaces for types and for data (terms).
Here’s an artificial example: the code below defines `T :: Type` and `U :: Type -> Type` in the type namespace, and `U :: Int -> T` and `T :: a -> U a` in the data namespace:

```haskell
data T = U Int
data U a = T a
```

However, the boundary between type and data namespaces is becoming more blurred with extensions like `DataKinds` and `RequiredTypeArguments`.
It’s ideal to avoid ambiguity by giving types and data different names, but there is already a lot of existing code that uses the same name for both, such as the `Proxy` type:

```haskell
data Proxy a = Proxy
```

With the `ExplicitNamespaces` extension, you can choose to import only types.
Moreover, with the `PatternSynonyms` extension, you can select only the data constructors:

```haskell
-- This works in older GHC versions too (e.g., 9.12)
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
import qualified Data.Proxy as PT (type Proxy)
import qualified Data.Proxy as PD (pattern Proxy)

main :: IO ()
main = print (PD.Proxy :: PT.Proxy PD.Proxy)
```

However, writing `pattern` just to import data constructors feels a bit unnatural.
In GHC 9.14, `ExplicitNamespaces` has been extended so that you can now use `data` to import data constructors:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
import qualified Data.Proxy as PT (type Proxy)
import qualified Data.Proxy as PD (data Proxy)

main :: IO ()
main = print (PD.Proxy :: PT.Proxy PD.Proxy)
```

In the future, you’ll be able to import everything from either the type or data namespace using double dots `..`, but this is not implemented in GHC 9.14 yet ([#25901](https://gitlab.haskell.org/ghc/ghc/-/issues/25901)):

```haskell
-- Not yet available in GHC 9.14
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
import qualified Data.Proxy as PT (type ..)
import qualified Data.Proxy as PD (data ..)

main :: IO ()
main = print (PD.Proxy :: PT.Proxy PD.Proxy)
```

As a side effect, using `pattern` in import/export lists is now deprecated (though it’s not planned to be removed).
GHC 9.14 introduces a new warning option, `-Wpattern-namespace-specifier` (included in `-Wcompat`), which triggers a warning when `pattern` is used in import/export lists.

## Bug fix: using a data type in a kind without `DataKinds` no longer allowed

* [#22141: GHC-9.4 accepts "data" in kinds without DataKinds · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/22141)

The following code uses the data type `Nat` as a kind, so the `DataKinds` extension is required.
However, starting with GHC 9.4, it was incorrectly accepted even without `DataKinds`:

```haskell
import Data.Kind (Type)
import GHC.TypeNats (Nat)

type T :: Nat -> Type
data T a = T
```

This bug has now been fixed.

## The `MonadComprehensions` extension now implies `ParallelListComp`

* [#25645: MonadComprehensions does not imply ParallelListComp? · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25645)

The `MonadComprehensions` extension was [documented](https://downloads.haskell.org/ghc/9.12.2/docs/users_guide/exts/monad_comprehensions.html#extension-MonadComprehensions) to imply `ParallelListComp`, but in practice it did not.

Starting with GHC 9.14, the following code now compiles:

```haskell
{-# LANGUAGE MonadComprehensions #-}

main :: IO ()
main = print [(x,y) | x <- [1,2,3] | y <- ["Alpha","Bravo","Charlie"]]
```

## You can now use visible `forall` in data constructors

* [ghc-proposals/proposals/0281-visible-forall.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst)
* [#25127: Visible forall in GADTs · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25127)
* [6.4.19. Required type arguments — Glasgow Haskell Compiler 9.14.0.20250819 User's Guide](https://downloads.haskell.org/ghc/9.14.0.20250819/docs/users_guide/exts/required_type_arguments.html#visible-forall-in-gadts)

GADTs now support visible `forall` (i.e., `forall a ->`).
The `ProxyList'` type from the previously written article [Playing with Visible Forall in GHC 9.10](./2024-05-11-playing-with-visible-forall.md) can now be rewritten as follows:

```haskell
{-# LANGUAGE RequiredTypeArguments #-}

type ProxyList :: [k] -> Type
data ProxyList xs where
  PNil :: ProxyList '[]
  PCons :: forall x -> forall xs -> ProxyListI xs => ProxyList (x : xs)

class ProxyListI xs where
  proxyList :: ProxyList xs

instance ProxyListI '[] where
  proxyList = PNil

instance ProxyListI xs => ProxyListI (x : xs) where
  proxyList = PCons x xs
```

## Expanded SIMD Support in the x86 NCG Backend

* [#25487: x86 NCG SIMD: Implement pack/insert/broadcast/unpack for integer vectors · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25487)
* [#25643: x86 NCG SIMD: Implement arithmetic operations for integer vectors · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25643)
* [#26096: Better lowering for shuffleFloatX4# and shuffleDoubleX2# · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/26096)

SIMD (Single Instruction, Multiple Data) allows you to process multiple data elements with a single instruction, which is useful for writing high-performance code on modern CPUs.
GHC provides data types and primops for SIMD.

Until recently, GHC’s SIMD primitives were only available with the LLVM backend.
Progress has been made: in GHC 9.12, some SIMD types and operations became available on the x86 NCG backend.

In GHC 9.14, the set of types and operations supported on x86 NCG has expanded.
In addition, shuffling floating-point vectors no longer requires `-mavx` on x86 NCG (it was required in GHC 9.12).

```haskell
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import GHC.Prim
import GHC.Exts
import GHC.Int

data FloatX4 = FloatX4 FloatX4#

packFloatX4 :: (Float, Float, Float, Float) -> FloatX4
packFloatX4 (F# x0, F# x1, F# x2, F# x3) = FloatX4 (packFloatX4# (# x0, x1, x2, x3 #))

unpackFloatX4 :: FloatX4 -> (Float, Float, Float, Float)
unpackFloatX4 (FloatX4 v) = case unpackFloatX4# v of (# x0, x1, x2, x3 #) -> (F# x0, F# x1, F# x2, F# x3)

plusFloatX4 :: FloatX4 -> FloatX4 -> FloatX4
plusFloatX4 (FloatX4 u) (FloatX4 v) = FloatX4 (plusFloatX4# u v)

-- In GHC 9.14, shuffleFloatX4# works without -mavx
reverseFloatX4 :: FloatX4 -> FloatX4
reverseFloatX4 (FloatX4 v) = FloatX4 (shuffleFloatX4# v v (# 3#, 2#, 1#, 0# #))

-- In GHC 9.14, Int32X4# and similar are available on x86 NCG
data Int32X4 = Int32X4 Int32X4#

packInt32X4 :: (Int32, Int32, Int32, Int32) -> Int32X4
packInt32X4 (I32# x0, I32# x1, I32# x2, I32# x3) = Int32X4 (packInt32X4# (# x0, x1, x2, x3 #))

unpackInt32X4 :: Int32X4 -> (Int32, Int32, Int32, Int32)
unpackInt32X4 (Int32X4 v) = case unpackInt32X4# v of (# x0, x1, x2, x3 #) -> (I32# x0, I32# x1, I32# x2, I32# x3)

plusInt32X4 :: Int32X4 -> Int32X4 -> Int32X4
plusInt32X4 (Int32X4 u) (Int32X4 v) = Int32X4 (plusInt32X4# u v)

reverseInt32X4 :: Int32X4 -> Int32X4
reverseInt32X4 (Int32X4 v) = Int32X4 (shuffleInt32X4# v v (# 3#, 2#, 1#, 0# #))

main :: IO ()
main = do
  let a = packFloatX4 (1.0, 2.0, 3.0, 4.0)
      b = packFloatX4 (0.1, 0.2, 0.3, 0.4)
      c = plusFloatX4 a b
  print (unpackFloatX4 c)
  print (unpackFloatX4 (reverseFloatX4 c))
  let d = packInt32X4 (10, 20, 30, 40)
      e = packInt32X4 (1, 2, 3, 4)
      f = plusInt32X4 d e
  print (unpackInt32X4 f)
  print (unpackInt32X4 (reverseInt32X4 f))
```

At this stage, the vector width supported on x86 NCG is still limited to 128 bits.

## GHCi Now Supports Multiple Home Units

* [Making GHCi compatible with multiple home units - Well-Typed: The Haskell Consultants](https://well-typed.com/blog/2025/06/ghci-multiple-home-units/)
* [#20889: MHU: Most of GHCi commands do not work · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/20889)

GHCi can now handle projects composed of multiple units (libraries and executables) more effectively.
I don’t fully understand all the internals yet, so I’ll demonstrate by experimenting.

First, create a sample project:

```
$ mkdir mhu-example && cd mhu-example
$ cabal init -n --tests --libandexe -d "base >=4.19 && <4.23"
```

Now, try running it in the REPL:

```
$ cabal repl -w ghc-9.12 exe:mhu-example
Resolving dependencies...
Build profile: -w ghc-9.12.2 -O1
In order, the following will be built (use -v for more details):
 - mhu-example-0.1.0.0 (lib) (configuration changed)
 - mhu-example-0.1.0.0 (interactive) (exe:mhu-example) (configuration changed)
Configuring library for mhu-example-0.1.0.0...
Preprocessing library for mhu-example-0.1.0.0...
Building library for mhu-example-0.1.0.0...
[1 of 1] Compiling MyLib            ( src/MyLib.hs, dist-newstyle/build/x86_64-osx/ghc-9.12.2/mhu-example-0.1.0.0/build/MyLib.o, dist-newstyle/build/x86_64-osx/ghc-9.12.2/mhu-example-0.1.0.0/build/MyLib.dyn_o )
Configuring executable 'mhu-example' for mhu-example-0.1.0.0...
Preprocessing executable 'mhu-example' for mhu-example-0.1.0.0...
GHCi, version 9.12.2: https://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Main             ( app/Main.hs, interpreted )
Ok, one module loaded.
ghci> main
Hello, Haskell!
someFunc
ghci>
```

Looks good.

While keeping the REPL open, modify `src/MyLib.hs` as follows:

```haskell
module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc, revised"
```

Reload in GHCi (`:r`) and run again—the output **does not change**:

```
ghci> :r
Ok, one module reloaded.
ghci> main
Hello, Haskell!
someFunc
```

The linked article explains this in detail, but this illustrates the limitation of older GHC versions.

Now, try GHC 9.14 with `--enable-multi-repl`, also specifying the library `lib:mhu-example` to Cabal:

```
$ cabal repl -w ghc-9.14 --enable-multi-repl exe:mhu-example lib:mhu-example
Resolving dependencies...
Build profile: -w ghc-9.14.0.20250819 -O1
In order, the following will be built (use -v for more details):
 - mhu-example-0.1.0.0 (interactive) (lib) (configuration changed)
 - mhu-example-0.1.0.0 (interactive) (exe:mhu-example) (configuration changed)
Configuring library for mhu-example-0.1.0.0...
Preprocessing library for mhu-example-0.1.0.0...
Configuring executable 'mhu-example' for mhu-example-0.1.0.0...
Preprocessing executable 'mhu-example' for mhu-example-0.1.0.0...
GHCi, version 9.14.0.20250819: https://www.haskell.org/ghc/  :? for help
[1 of 3] Compiling MyLib            ( src/MyLib.hs, interpreted )[mhu-example-0.1.0.0-inplace]
[2 of 3] Compiling Main             ( app/Main.hs, interpreted )[mhu-example-0.1.0.0-inplace-mhu-example]
Ok, two modules loaded.
ghci> Main.main
Hello, Haskell!
someFunc, revised
```

Great.

Now, while keeping the REPL open, modify `src/MyLib.hs` again:

```haskell
module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc, revised^2"
```

Reloading the REPL now **reflects changes to the library**:

```
ghci> :r
[1 of 3] Compiling MyLib            ( src/MyLib.hs, interpreted )[mhu-example-0.1.0.0-inplace] [Source file changed]
Ok, two modules reloaded.
ghci> Main.main
Hello, Haskell!
someFunc, revised^2
```

## Libraries

We also cover changes to libraries, mainly the `base` package.

### `fail` Now Includes `HasCallStack`

* [Add HasCallStack to Control.Monad.fail · Issue #327 · haskell/core-libraries-committee](https://github.com/haskell/core-libraries-committee/issues/327)

`HasCallStack` is a lightweight mechanism for capturing stack traces.
Attaching it to functions that may fail (i.e., functions that may call `error`) helps with debugging.

In GHC 9.14, the `fail` function in the `MonadFail` class now has `HasCallStack`.
The `fail` function is called when a pattern match in a `do` block fails.
Here’s an example:

```haskell
someFunc :: [Int] -> IO ()
someFunc xs = do
  [a,b] <- pure xs
  print (a + b)

main :: IO ()
main = do
  someFunc [1,3]
  someFunc [2]
```

Running it with GHC 9.12:

```
$ runghc-9.12 fail.hs
4
fail.hs: Uncaught exception ghc-internal:GHC.Internal.Exception.ErrorCall:

user error (Pattern match failure in 'do' block at fail.hs:3:3-7)
```

Running the same code with GHC 9.14:

```
$ runghc-9.14 fail.hs
4
fail.hs: Uncaught exception ghc-internal:GHC.Internal.Exception.ErrorCall:

user error (Pattern match failure in 'do' block at fail.hs:3:3-7)

HasCallStack backtrace:
  throwIO, called at libraries/ghc-internal/src/GHC/Internal/Control/Monad/Fail.hs:66:12 in ghc-internal:GHC.Internal.Control.Monad.Fail
  a type signature in an instance, called at libraries/ghc-internal/src/GHC/Internal/Control/Monad/Fail.hs:65:13 in ghc-internal:GHC.Internal.Control.Monad.Fail
  a do statement, called at fail.hs:3:3 in main:Main
```

In GHC 9.14, the error message now includes a stack trace.
In this particular example, the benefit may be limited because the failing pattern match location (`fail.hs:3:3-7`) is already reported.

By adding `HasCallStack` to a function, you can include information about where the function was called. For example, adding it to `someFunc` shows the caller location:

```haskell
import GHC.Stack (HasCallStack)

someFunc :: HasCallStack => [Int] -> IO ()
someFunc xs = do
  [a,b] <- pure xs
  print (a + b)

main :: IO ()
main = do
  someFunc [1,3]
  someFunc [2]
```

Running with GHC 9.12:

```
$ runghc-9.12 fail2.hs
4
fail2.hs: Uncaught exception ghc-internal:GHC.Internal.Exception.ErrorCall:

user error (Pattern match failure in 'do' block at fail2.hs:5:3-7)
```

Running with GHC 9.14:

```
$ runghc-9.14 fail2.hs
4
fail2.hs: Uncaught exception ghc-internal:GHC.Internal.Exception.ErrorCall:

user error (Pattern match failure in 'do' block at fail2.hs:5:3-7)

HasCallStack backtrace:
  throwIO, called at libraries/ghc-internal/src/GHC/Internal/Control/Monad/Fail.hs:66:12 in ghc-internal:GHC.Internal.Control.Monad.Fail
  a type signature in an instance, called at libraries/ghc-internal/src/GHC/Internal/Control/Monad/Fail.hs:65:13 in ghc-internal:GHC.Internal.Control.Monad.Fail
  a do statement, called at fail2.hs:5:3 in main:Main
  someFunc, called at fail2.hs:11:3 in main:Main
```

In GHC 9.14, the stack trace now includes the location in the `main` function (`fail2.hs:11:3`).

### Introduction of `Data.Enum.enumerate`

* [enumerate Function · Issue #306 · haskell/core-libraries-committee](https://github.com/haskell/core-libraries-committee/issues/306)

A new function `enumerate` has been added that returns a list containing all elements of an enumeration type. Previously, many people would use the pattern `[minBound..maxBound]`.

```haskell
import Data.Enum

data Color = Red | Green | Blue deriving (Show, Enum, Bounded)

main :: IO ()
main = do
  print ([minBound..maxBound] :: [Color])
  print (enumerate @Color)
```

```
$ runghc-9.14 enum.hs 
[Red,Green,Blue]
[Red,Green,Blue]
```

# Extra: My Contributions

I (Mizuki) am jotting down a record of my contributions—bug reports, fixes, etc.—that made it into GHC 9.14.

* Enable compilation of integer SIMD primitives on the x86 NCG backend.
    * [#25487: x86 NCG SIMD: Implement pack/insert/broadcast/unpack for integer vectors · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25487)
    * [#25643: x86 NCG SIMD: Implement arithmetic operations for integer vectors · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25643)
* Enable compilation of floating-point shuffle on x86 NCG using SSE2 instructions (instead of AVX).
    * [#26096: Better lowering for shuffleFloatX4# and shuffleDoubleX2# · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/26096)
* Fix for arithmetic right shift on AArch64 NCG.
    * [#26061: Sub-word arithmetic right shift with AArch64 NCG · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/26061)
    * Scheduled to be backported to GHC 9.10.3 and also to the 9.12 series.
* Fix for `bswap64` on i386 NCG.
    * [!14363: x86 NCG: Fix code generation of bswap64 on i386 · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/14363)
    * Scheduled to be backported to GHC 9.10.3 and also to the 9.12 series.
* Fix for LLVM version detection issues.
    * [!13763: Fix LLVM version detection (#25606) · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/13763)
    * Backported to GHC 9.10.2 and scheduled for backport to the 9.12 series.

I do these contributions as a hobby.
I’m grateful that a few people have supported me via GitHub Sponsors:

* [Sponsor @minoki on GitHub Sponsors](https://github.com/sponsors/minoki)
    * At the time of writing, I’m supported by @toyboot4e and @kevin-kmetz.

Thank you for reading, and enjoy your Haskell journey!
