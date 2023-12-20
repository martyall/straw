{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Examples.Snarkl
  ( arr_ex,
    p1,
  )
where

import Snarkl.Language (Comp, TExp, Ty (..), arr, forall, fromRational, get, return, set, (+), (>>), (>>=))
import Prelude (Rational, fromInteger, ($))

arr_ex :: TExp 'TField Rational -> Comp 'TField
arr_ex x = do
  a <- arr 2
  forall [0 .. 1] (\i -> set (a, i) x)
  y <- get (a, 0)
  z <- get (a, 1)
  return $ y + z

p1 = arr_ex 1.0
