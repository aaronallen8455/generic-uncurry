{-# LANGUAGE
    TypeFamilies
  , TypeOperators
  , GADTs
  , DataKinds
  , UndecidableInstances
  , ScopedTypeVariables
  , TypeApplications
  , PolyKinds
  , FlexibleInstances
  , MultiParamTypeClasses #-}
module GUncurry
  ( guncurry
  ) where

import           Data.Kind
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits (ErrorMessage(..), TypeError)

guncurry :: forall f tuple numArgs rep meta1 meta2.
            ( Generic tuple
            , D1 meta1 (C1 meta2 rep) ~ Rep tuple
            , RepToHList rep
            , numArgs ~ CountArgs f
            , NatSing numArgs
            , ArgsList f ~ RepList rep
            , Apply f numArgs
            )
         => f -> tuple -> Codomain f
guncurry f tuple = apply nArgs f (toHList rep)
  where
    M1 (M1 rep) = from tuple
    nArgs = natSing $ Proxy @numArgs

--------------------------------------------------------------------------------
-- Peano numbers
--------------------------------------------------------------------------------

data Nat = S Nat | Z

data SNat :: Nat -> Type where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

class NatSing (n :: Nat) where
  natSing :: Proxy n -> SNat n

instance NatSing 'Z where
  natSing _ = SZ

instance NatSing n => NatSing ('S n) where
  natSing _ = SS $ natSing (Proxy @n)

--------------------------------------------------------------------------------
-- Heterogenous list
--------------------------------------------------------------------------------

data HList :: [Type] -> Type where
  Nil :: HList '[]
  Cons ::  a -> HList tys -> HList (a ': tys)

catHList :: HList a -> HList b -> HList (Append a b)
catHList Nil xs = xs
catHList (Cons a as) bs = Cons a $ catHList as bs

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

class RepToHList rep where
  toHList :: rep x -> HList (RepList rep)

instance RepToHList (S1 m (Rec0 a)) where
  toHList (M1 (K1 a)) = Cons a Nil

instance (RepToHList a, RepToHList b) => RepToHList (a :*: b) where
  toHList (a :*: b) = catHList (toHList a) (toHList b)

class Apply f (n :: Nat) where
  apply :: SNat n -> f -> HList (ArgsList f) -> Codomain f

instance (CountArgs a ~ 'Z, Codomain a ~ a) => Apply a 'Z where
  apply SZ a Nil = a

instance Apply b n => Apply (a -> b) ('S n) where
  apply (SS n) f (Cons x xs) = apply n (f x) xs

--------------------------------------------------------------------------------
-- Type families
--------------------------------------------------------------------------------

type family CountArgs f :: Nat where
  CountArgs (a -> b) = 'S (CountArgs b)
  CountArgs z = 'Z

type family Codomain f :: Type where
  Codomain (a -> b) = Codomain b
  Codomain b = b

type family RepList (rep :: Type -> Type) :: [Type] where
  RepList (a :*: b) = Append (RepList a) (RepList b)
  RepList (S1 m (Rec0 x)) = '[x]
  RepList x = TypeError ('Text "Cannot form type list. Is this a Rep?")

type family Append (a :: [Type]) (b :: [Type]) :: [Type] where
  Append '[] b = b
  Append (a ': as) bs = a ': Append as bs

type family ArgsList f :: [Type] where
  ArgsList (a -> b) = a ': ArgsList b
  ArgsList x = '[]

