{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Examples.Lens where

import qualified Categorifier.Categorify as Categorify
import Categorifier.Client
import Control.Lens
import Snarkl.Language (Ty (TField), pair, (>>=))
import qualified Snarkl.Language.SyntaxMonad as Snarkl
import Straw
import Prelude (Foldable (foldMap, foldl), Rational, fromInteger, mempty, (<>))

data Point = Point {_x :: Rational, _y :: Rational}

type instance SnarklTy Point = SnarklTy (Rep Point)

$(deriveHasRep ''Point)
$(makeLenses ''Point)

data Atom = Atom {_elem :: Rational, _point :: Point, _foo :: Rational}

type instance SnarklTy Atom = SnarklTy (Rep Atom)

$(deriveHasRep ''Atom)
$(makeLenses ''Atom)

data ConsList a = Nil | Cons a (ConsList a)

$(deriveHasRep ''ConsList)

instance Foldable ConsList where
  foldl _ z Nil = z
  foldl f z (Cons x xs) = foldl f (f z x) xs
  {-# INLINEABLE foldl #-}

  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs
  {-# INLINEABLE foldMap #-}

simpleLens :: Snarkl.Comp 'TField
simpleLens =
  let prog :: Point -> Rational
      prog p =
        let atom = Atom 32 p 2
            f :: Atom -> Rational -> Atom
            f a n = a & elem +~ n
         in f atom 10 ^. elem

      compiledProg :: Straw Point Rational
      compiledProg = Categorify.expression prog
   in do
        x <- Snarkl.fresh_input
        y <- Snarkl.fresh_input
        p <- pair x y
        runStraw compiledProg p

complicatedLens :: Snarkl.Comp 'TField
complicatedLens =
  let prog :: Atom -> Rational
      prog atom =
        let f :: Atom -> Rational -> Atom
            f a n = a & elem +~ n
            ls = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
         in foldl f atom ls ^. elem
      compiledProg :: Straw Atom Rational
      compiledProg = Categorify.expression prog
   in do
        x <- Snarkl.fresh_input
        y <- Snarkl.fresh_input
        z <- Snarkl.fresh_input
        w <- Snarkl.fresh_input
        pyz <- pair y z
        pxyw <- pair pyz w
        p <- pair x pxyw
        runStraw compiledProg p
