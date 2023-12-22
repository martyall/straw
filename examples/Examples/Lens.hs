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
import Data.Vec.Lazy (Vec (..))
import Snarkl.Language (Ty (TField), pair, (>>=))
import qualified Snarkl.Language.SyntaxMonad as Snarkl
import Straw
import Prelude (Rational, foldl, fromInteger)

data Point = Point {_x :: Rational, _y :: Rational}

type instance SnarklTy Point = SnarklTy (Rep Point)

$(deriveHasRep ''Point)
$(makeLenses ''Point)

data Atom = Atom {_elem :: Rational, _point :: Point, _foo :: Rational}

type instance SnarklTy Atom = SnarklTy (Rep Atom)

$(deriveHasRep ''Atom)
$(makeLenses ''Atom)

simpleLens :: Snarkl.Comp 'TField
simpleLens =
  let prog :: (Rational, Rational) -> Rational
      prog (x, y) =
        let p = Point x y
            atom = Atom 32 p 1
            f :: Atom -> Rational -> Atom
            f a n = a & elem +~ n
         in f atom 10 ^. elem

      compiledProg :: Straw (Rational, Rational) Rational
      compiledProg = Categorify.expression prog
   in do
        x <- Snarkl.fresh_input
        y <- Snarkl.fresh_input
        p <- pair x y
        runStraw compiledProg p

{-
complicatedLens :: Snarkl.Comp 'TField
complicatedLens =
  let prog :: (Rational, (Rational, Rational)) -> Rational
      prog (x, (y, z)) =
        let p = Point x y
            atom :: Atom
            atom = Atom z p 1
            f :: Atom -> Rational -> Atom
            f a n = a & elem +~ n
            ls = 1 ::: 2 ::: 3 ::: VNil
         in foldl f atom ls ^. elem
      compiledProg :: Straw (Rational, (Rational, Rational)) Rational
      compiledProg = Categorify.expression prog
   in do
        x <- Snarkl.fresh_input
        y <- Snarkl.fresh_input
        z <- Snarkl.fresh_input
        yz <- pair y z
        p <- pair x yz
        runStraw compiledProg p
-}
