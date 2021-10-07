{-# LANGUAGE AllowAmbiguousTypes #-}
module Haskiann where

import Data.Kind

data Nat :: Type where
  Z :: Nat 
  S :: Nat -> Nat

type family (:+) (m :: Nat) (n :: Nat) where
  'Z :+ n = n
  ('S m) :+ n = 'S (m :+ n)

data SNat :: Nat -> Type where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n) 

data Vec :: Type -> Nat -> Type where
  Nil :: forall (a :: Type). Vec a 'Z
  Cons :: forall (a :: Type) (n :: Nat). a -> Vec a n -> Vec a ('S n)  

type family (:++) (xs :: Vec a m) (ys :: Vec a n) :: Vec a (m :+ n) where
  'Nil         :++ ys = ys
  ('Cons x xs) :++ ys = 'Cons x (xs :++ ys) 

data SVec :: forall (a :: Type) (n :: Nat). Vec a n -> Type where
  SNil :: SVec 'Nil
  SCons :: forall (a :: Type) (n :: Nat) (as :: Vec Type n). a -> SVec as -> SVec ('Cons a as)

(++) :: SVec xs -> SVec ys -> SVec (xs :++ ys)
SNil         ++ ys = ys
(SCons x xs) ++ ys = SCons x (xs ++ ys)

-- Extended version of the graph datatype described in "An Initial Algebra Approach
-- to Directed Acyclic Graphs" by Jeremy Gibbons.
data Graph :: forall (m :: Nat) (n :: Nat). Vec Type m -> Vec Type n -> Type where
  Vert :: forall (m :: Nat) (n :: Nat) (ms :: Vec Type m) (ns :: Vec Type n)
       .  SVec ms
       -> SVec ns
       -> Graph ms ns
  Edge :: forall (a :: Type). Graph ('Cons a 'Nil) ('Cons a 'Nil)
  Beside :: Graph ms ns -> Graph ps qs -> Graph (ms :++ ps) (ns :++ qs)
  Before :: Graph ms ns -> Graph ns ps -> Graph ms ps
  Empty :: Graph 'Nil 'Nil
  Swap :: forall (m :: Nat) (n :: Nat) (ms :: Vec Type m) (ns :: Vec Type n)
       .  SNat m
       -> SNat n
       -> Graph (ms :++ ns) (ns :++ ms)
  Feedback :: forall 
              (m :: Nat) 
              (n :: Nat) 
              (s :: Nat) 
              (ms :: Vec Type m) 
              (ns :: Vec Type n) 
              (ss :: Vec Type s)
           .  SNat m
           -> SNat n
           -> SNat s
           -> Graph (ms :++ ss) (ns :++ ss)
           -> Graph ms ns

