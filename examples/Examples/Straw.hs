{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Examples.Straw
  ( mult_ex,
    arr_ex,
    p1,
    p2,
    comp1,
    comp2,
    -- test1,
  )
where

import qualified Categorifier.Categorify as Categorify
import Categorifier.Vec.Client ()
import Data.Bool (bool)
import Data.Either (fromLeft)
import Data.Vec.Lazy (Vec (..))
import qualified Snarkl.Language.SyntaxMonad as Snarkl
import Straw
import "snarkl" Snarkl.Language (fromRational, (>>=))
import Prelude (Bool (..), Either (..), Rational, fromInteger, sum, ($), (*), (+), (.))

-- * Basic

mult_ex :: Rational -> Rational -> Rational
mult_ex x y = x * y

-- | Compiling to Categories doesn't (yet) support direct indexing. But in this
--   case we can use a simple fold, which is much more common/recommended in
--   functional programs anyway.
arr_ex :: Rational -> Rational
arr_ex x =
  let a = x ::: x ::: VNil
   in -- in (a ! 0) + (a ! 1)
      sum a

p1 = runStraw (Categorify.expression arr_ex) 1.0

p2 =
  Snarkl.fresh_input
    >>= runStraw (Categorify.expression $ \(x :: Rational) -> x + x)

comp1 :: Either Bool Rational
comp1 = Left False

comp2 :: Either Bool Rational
comp2 = Right 0

-- | __TODO__: Needs @`ConCat.Category.ClosedCat` `Straw`@
test1 =
  Snarkl.fresh_input
    >>= runStraw
      (Categorify.expression $ fromLeft False . bool comp2 comp1)
