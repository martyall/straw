{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | A category for Snarkl
module Straw
  ( Straw (..),
    SnarklTy,
  )
where

import Categorifier.Category
  ( UnsafeCoerceCat (..),
  )
import qualified Categorifier.Category as Categorifier
import Categorifier.Vec.Client ()
import qualified ConCat.Category as ConCat
import Data.Bool (bool)
import qualified Data.Constraint as Constraint
import Data.Functor.Const (Const (..))
import Data.Proxy (Proxy (..))
import Data.Type.Nat (Nat (..))
import qualified Data.Type.Nat as Nat
import Data.Typeable (Typeable)
import Data.Vec.Lazy (Vec (..))
import Snarkl.Language (fromRational, return, (>>), (>>=))
import qualified Snarkl.Language as Snarkl
import Prelude (Bool, Either, Integer, Rational, error, fromInteger, ($), (+), (.))

data Straw a b = Straw
  {runStraw :: Snarkl.TExp (SnarklTy a) Rational -> Snarkl.Comp (SnarklTy b)}

instance Categorifier.RepCat Straw Rational (Integer, Integer) where
  abstC = Straw $ \p -> do
    n <- Snarkl.fst_pair p
    d <- Snarkl.snd_pair p
    Snarkl.return $ n Snarkl./ d

  -- __FIXME__: This doesn't guarantee we have an integer in the numerator.
  reprC = Straw $ \r -> Snarkl.pair r (Snarkl.fromRational 1)

instance
  (Nat.SNatI n, LiftSnarklTy a) =>
  Categorifier.RepCat Straw (Vec ('S ('S n)) a) (a, Vec ('S n) a)
  where
  abstC = Straw $ \p -> do
    let last = Nat.reflectToNum (Proxy @n)
    a <- Snarkl.arr $ last + 2
    Snarkl.fst_pair p >>= Snarkl.set (a, 0)
    Snarkl.snd_pair p
      >>= \a' ->
        Snarkl.forall [0 .. last] $
          \i -> Snarkl.get (a', i) >>= Snarkl.set (a, i + 1)
    return a
  reprC = Straw $ \a -> do
    let last = Nat.reflectToNum (Proxy :: Proxy n)
    let subLen = last + 1
    h <- Snarkl.get (a, 0)
    t <- Snarkl.arr subLen
    Snarkl.forall [0 .. last] (\i -> Snarkl.get (a, i + 1) >>= Snarkl.set (t, i))
    Snarkl.pair h t

instance
  (LiftSnarklTy a) =>
  Categorifier.RepCat Straw (Vec ('S 'Z) a) (a, Vec 'Z a)
  where
  abstC = Straw $ \p -> do
    a <- Snarkl.arr 1
    Snarkl.fst_pair p >>= Snarkl.set (a, 0)
    return a
  reprC = Straw $ \a -> do
    h <- Snarkl.get (a, 0)
    Snarkl.pair h Snarkl.unit

instance (LiftSnarklTy a) => Categorifier.RepCat Straw (Vec 'Z a) () where
  abstC = Straw return
  reprC = Straw return

-- | Mapping from Haskell types to Snarkl types.
type family SnarklTy a :: Snarkl.Ty

type instance SnarklTy () = 'Snarkl.TUnit

type instance SnarklTy (a, b) = 'Snarkl.TProd (SnarklTy a) (SnarklTy b)

-- type instance SnarklTy (a -> b) = 'Snarkl.TMu TF
type instance SnarklTy Bool = 'Snarkl.TBool

type instance SnarklTy (Either a b) = 'Snarkl.TSum (SnarklTy a) (SnarklTy b)

type instance SnarklTy Integer = 'Snarkl.TField

type instance SnarklTy Rational = 'Snarkl.TField

type instance SnarklTy (Vec ('S n) a) = 'Snarkl.TArr (SnarklTy a)

-- | Snarkl only supports non-empty sequences, but this exists for `Categorifier.RepCat`.
type instance SnarklTy (Vec 'Z a) = 'Snarkl.TUnit

type instance SnarklTy (a -> b) = 'Snarkl.TFun (SnarklTy a) (SnarklTy b)

type instance SnarklTy (Const Rational b) = 'Snarkl.TField

instance (SnarklTy a ~ SnarklTy b) => Categorifier.UnsafeCoerceCat Straw a b where
  unsafeCoerceK = Straw return

-- | This class exists because we can`t do @`Typeable` . `SnarklTy`@. It has a
--   universal instance.
class (Snarkl.Derive (SnarklTy a), Typeable (SnarklTy a), Snarkl.Zippable (SnarklTy a)) => LiftSnarklTy a

instance (Snarkl.Derive (SnarklTy a), Typeable (SnarklTy a), Snarkl.Zippable (SnarklTy a)) => LiftSnarklTy a

instance ConCat.OpCon (ConCat.Coprod Straw) (ConCat.Sat LiftSnarklTy) where
  inOp = ConCat.Entail (Constraint.Sub Constraint.Dict)

instance ConCat.OpCon (ConCat.Exp Straw) (ConCat.Sat LiftSnarklTy) where
  inOp = ConCat.Entail (Constraint.Sub Constraint.Dict)

instance ConCat.OpCon (ConCat.Prod Straw) (ConCat.Sat LiftSnarklTy) where
  inOp = ConCat.Entail (Constraint.Sub Constraint.Dict)

instance ConCat.Category Straw where
  type Ok Straw = LiftSnarklTy -- <whatever class SnarklTy ends up in, probably>
  id = Straw Snarkl.return
  Straw f . Straw g = Straw $ \x -> g x Snarkl.>>= f

instance ConCat.ClosedCat Straw where
  -- curry :: (TExp ('TProd a b) -> Comp c) -> (TExp a -> Comp ('Mu TF))
  curry :: (ConCat.Ok3 Straw a b c) => Straw (ConCat.Prod Straw a b) c -> Straw a (ConCat.Exp Straw b c)
  curry (Straw f) = Straw $ Snarkl.curry f
  uncurry :: (ConCat.Ok3 Straw a b c) => Straw a (ConCat.Exp Straw b c) -> Straw (ConCat.Prod Straw a b) c
  uncurry (Straw f) = Straw $ Snarkl.uncurry f

instance ConCat.CoproductCat Straw where
  inl :: (ConCat.Ok2 Straw a b) => Straw a (ConCat.Coprod Straw a b)
  inl = Straw Snarkl.inl
  inr = Straw Snarkl.inr
  jam = Straw $ Snarkl.case_sum Snarkl.return Snarkl.return

instance ConCat.MonoidalPCat Straw where
  Straw f *** Straw g = Straw $ \p -> do
    a <- Snarkl.fst_pair p Snarkl.>>= f
    b <- Snarkl.snd_pair p Snarkl.>>= g
    Snarkl.pair a b

instance ConCat.MonoidalSCat Straw where
  Straw f +++ Straw g =
    Straw $
      Snarkl.case_sum
        (\a -> f a Snarkl.>>= Snarkl.inl)
        (\b -> g b Snarkl.>>= Snarkl.inr)

instance ConCat.ProductCat Straw where
  exl = Straw Snarkl.fst_pair
  exr = Straw Snarkl.snd_pair
  dup = Straw $ \x -> Snarkl.pair x x

instance ConCat.BraidedPCat Straw

instance ConCat.DistribCat Straw where
  distl = Straw $ \p -> do
    a <- Snarkl.fst_pair p
    Snarkl.snd_pair p
      >>= Snarkl.case_sum
        (\u -> Snarkl.pair a u >>= Snarkl.inl)
        (\v -> Snarkl.pair a v >>= Snarkl.inr)

instance ConCat.BoolCat Straw where
  notC = Straw $ Snarkl.return . Snarkl.not
  andC = Straw $ \p -> do
    a <- Snarkl.fst_pair p
    b <- Snarkl.snd_pair p
    Snarkl.return $ a Snarkl.&& b
  orC = Straw $ \p -> do
    a <- Snarkl.fst_pair p
    b <- Snarkl.snd_pair p
    Snarkl.return . Snarkl.not $ Snarkl.not a Snarkl.&& Snarkl.not b
  xorC = Straw $ \p -> do
    a <- Snarkl.fst_pair p
    b <- Snarkl.snd_pair p
    Snarkl.return $ Snarkl.xor a b

instance ConCat.TerminalCat Straw

instance ConCat.ConstCat Straw Bool where
  const = Straw . bool (\_ -> return Snarkl.false) (\_ -> return Snarkl.true)

instance ConCat.ConstCat Straw Rational where
  const a = Straw $ \_ -> return $ Snarkl.fromRational a

instance ConCat.ConstCat Straw () where
  const () = Straw $ \_ -> return $ Snarkl.unit

instance (Nat.SNatI n) => ConCat.AddCat Straw (Vec ('S n)) Rational where
  sumAC = Straw $ \a ->
    Snarkl.iterM
      (Nat.reflectToNum (Proxy :: Proxy n))
      (\n' e -> Snarkl.get (a, n') >>= (return . (Snarkl.+ e)))
      0.0

instance
  (ConCat.Ok Straw a, Snarkl.Zippable (SnarklTy a)) =>
  ConCat.IfCat Straw a
  where
  ifC = Straw $ \p -> do
    b <- Snarkl.snd_pair p
    Snarkl.ifThenElse
      (Snarkl.fst_pair p)
      (Snarkl.fst_pair b)
      (Snarkl.snd_pair b)

instance ConCat.NumCat Straw Rational where
  addC = Straw $ \p -> do
    a <- Snarkl.fst_pair p
    b <- Snarkl.snd_pair p
    Snarkl.return $ a Snarkl.+ b
  mulC = Straw $ \p -> do
    a <- Snarkl.fst_pair p
    b <- Snarkl.snd_pair p
    Snarkl.return $ a Snarkl.* b
  subC = Straw $ \p -> do
    a <- Snarkl.fst_pair p
    b <- Snarkl.snd_pair p
    Snarkl.return $ a Snarkl.- b
  negateC = Straw $ Snarkl.return . Snarkl.negate
  powIC = error "no exponents"

instance ConCat.EqCat Straw Rational where
  equal = Straw $ \p -> do
    a <- Snarkl.fst_pair p
    b <- Snarkl.snd_pair p
    Snarkl.return $ Snarkl.eq a b

cat ::
  (Typeable (SnarklTy a)) =>
  (Typeable (SnarklTy b)) =>
  Snarkl.TExp (SnarklTy (a -> b)) Rational ->
  Straw a b
cat f = Straw $ \x -> return $ Snarkl.TEApp f x

lowerCat ::
  (Typeable (SnarklTy a)) =>
  (Typeable (SnarklTy b)) =>
  Straw a b ->
  Snarkl.Comp (SnarklTy (a -> b))
lowerCat (Straw f) = Snarkl.lambda f
