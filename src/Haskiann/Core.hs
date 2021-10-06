module Haskiann.Core where

import Data.Kind

data Nat :: Type where
  Z :: Nat 
  S :: Nat -> Nat

type family (:+) (n :: Nat) (m :: Nat) :: Nat where
  'Z :+ n = n
  ('S n) :+ m = 'S (n :+ m)

data SNat :: Nat -> Type where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n) 

data Vec :: Type -> Nat -> Type where
  Nil :: forall (a :: Type). Vec a 'Z
  Cons :: forall (a :: Type) (n :: Nat). a -> Vec a n -> Vec a ('S n)  

data SVec :: forall (a :: Type) (n :: Nat). Vec a n -> Type where
  SNil :: SVec 'Nil
  SCons :: forall (a :: Type) (n :: Nat) (as :: Vec Type n). a -> SVec as -> SVec ('Cons a as)

-- Extended version of the graph datatype described in "An Initial Algebra Approach
-- to Directed Acyclic Graphs" by Jeremy Gibbons.
data Graph :: forall (m :: Nat) (n :: Nat). Vec Type m -> Vec Type n -> Type where
  Vert :: forall (m :: Nat) (n :: Nat) (ms :: Vec Type m) (ns :: Vec Type n).
       SVec ms ->
       SVec ns ->
       Graph ms ns
  Edge :: forall (x :: Type). Graph ('Cons x 'Nil) ('Cons x 'Nil)
  
