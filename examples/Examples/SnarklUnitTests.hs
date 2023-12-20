{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Examples.SnarklUnitTests where

import qualified Categorifier.Categorify as Categorify
import Categorifier.Vec.Client ()
import Snarkl.Language (Ty (TField), pair, (>>=))
import qualified Snarkl.Language.SyntaxMonad as Snarkl
import Straw
import Prelude (Bool (..), Rational, fromInteger, (&&), (*), (+), (-))

-- we need the because we enabled rebindable syntax
ifThenElse :: Bool -> a -> a -> a
ifThenElse True t _ = t
ifThenElse False _ f = f

prog1 :: Snarkl.Comp 'TField
prog1 =
  let prog :: (Bool, (Rational, Bool)) -> Rational
      prog (x, (y, z)) =
        let u = y + 2
            v = if z then y else y + 1
            w = if x then y else y + 2
         in u * u - (w * u * u * y * y * v)

      compiledProg :: Straw (Bool, (Rational, Bool)) Rational
      compiledProg = Categorify.expression prog
   in do
        x <- Snarkl.fresh_input
        y <- Snarkl.fresh_input
        z <- Snarkl.fresh_input
        yz <- pair y z
        p <- pair x yz
        runStraw compiledProg p

-- -- | 1. A standalone "program" in the expression language
-- prog1
--   = do { x <- fresh_input -- bool
--        ; y <- fresh_input -- int
--        ; z <- fresh_input -- bool
--        ; u <- return $ y + 2.0
--        ; v <- if return z then return y else return y
--        ; w <- if return x then return y else return y
--        ; return $ (u*u) - (w*u*u*y*y*v)
--        }

--  describe "if-then-else" $ do
--       it "1-1" $ test_comp Simplify prog1 [1,2,1] `shouldReturn` Right (negate 240)

-- -- | 6. 'times' test
-- prog6
--   = do { e <- fresh_input
--        ; a <- arr 100
--        ; times 1 (set (a,3) e)
--        ; x <- get (a,3)
--        ; return x
--        }

-- bool_prog9 :: Snarkl.Comp 'TField
bool_prog9 =
  let prog :: (Bool, Bool) -> Bool
      prog (x, y) = x && y

      compiledProg :: Straw (Bool, Bool) Bool
      compiledProg = Categorify.expression prog
   in do
        x <- Snarkl.fresh_input
        y <- Snarkl.fresh_input
        p <- pair x y
        runStraw compiledProg p

-- {-# INLINE catExpr #-}
-- catExpr expr =
--     let compiledExpr = Categorify.expression expr
--     in do
--         x <- Snarkl.fresh_input
--         y <- Snarkl.fresh_input
--         p <- pair x y
--         runStraw compiledExpr p

-- -- | 9. 'and' test
-- bool_prog9
--   = do { e1 <- fresh_input
--        ; e2 <- fresh_input
--        ; return (e1 && e2)
--        }
