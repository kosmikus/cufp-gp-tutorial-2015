{-# LANGUAGE GADTs, KindSignatures, DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module NP where

import GHC.Exts (Constraint)

-- data List a = Nil | Cons a (List a)

{-
data List :: * -> * where
  Nil   ::  List a
  Cons  ::  a -> List a -> List a

data HList :: [*] -> * where
  HNil  ::  HList '[]
  HCons ::  x -> HList xs -> HList (x ': xs)

infixr 5 `HCons`
-}

data NP :: (k -> *) -> [k] -> * where
  Nil  ::  NP f '[]
  (:*) ::  f x -> NP f xs -> NP f (x ': xs)

-- All :: (* -> Constraint) -> [*] -> Constraint

type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All c '[]       = ()
  All c (x ': xs) = (c x, All c xs)

class (f (g x)) => Compose f g x
instance (f (g x)) => Compose f g x

deriving instance All (Compose Show f) xs => Show (NP f xs)

infixr 5 :*

newtype I a = I a
  deriving Show

newtype K a b = K a
  deriving Show
