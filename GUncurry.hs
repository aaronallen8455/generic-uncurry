{-# LANGUAGE
    TypeFamilies
  , TypeOperators
  , GADTs
  , DataKinds
  , UndecidableInstances
  , FlexibleInstances
  , MultiParamTypeClasses #-}
module GUncurry
  ( guncurry
  ) where

import           Data.Kind
import           GHC.Generics
import           GHC.TypeLits (ErrorMessage(..), TypeError)

guncurry :: ( Generic tuple
            , D1 meta1 (C1 meta2 rep) ~ Rep tuple
            , RepToHList rep
            , args ~ ArgsList f
            , args ~ RepList rep
            , Apply f args
            )
         => f -> tuple -> Codomain f
guncurry f tuple = apply f (toHList rep)
  where
    M1 (M1 rep) = from tuple

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

class Apply f (args :: [Type]) where
  apply :: f -> HList args -> Codomain f

instance Codomain a ~ a => Apply a '[] where
  apply a Nil = a

instance Apply b args => Apply (a -> b) (a ': args) where
  apply f (Cons x xs) = apply (f x) xs

--------------------------------------------------------------------------------
-- Type families
--------------------------------------------------------------------------------

type family Codomain f :: Type where
  Codomain (a -> b) = Codomain b
  Codomain b = b

type family RepList (rep :: Type -> Type) :: [Type] where
  RepList (a :*: b) = Append (RepList a) (RepList b)
  RepList (S1 m (Rec0 x)) = '[x]
  RepList x = TypeError ('Text "This type is not compatible with 'guncurry'.")

type family Append (a :: [Type]) (b :: [Type]) :: [Type] where
  Append '[] b = b
  Append (a ': as) bs = a ': Append as bs

type family ArgsList f :: [Type] where
  ArgsList (a -> b) = a ': ArgsList b
  ArgsList x = '[]

