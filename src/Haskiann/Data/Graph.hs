{-# LANGUAGE AllowAmbiguousTypes #-}
module Haskiann.Data.Graph (Graph(..)) where

import Data.Kind
import Haskiann.Data.Nat
import Haskiann.Data.HVect

type family (++) xs ys where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

data Graph :: Nat -> Nat -> [Type] -> [Type] -> Type where
  Vert :: HVect ms m -> HVect ns n -> Graph m n ms ns
  Edge :: Graph 1 1 '[k] '[k]
  Beside :: Graph m n ms ns -> Graph p q ps qs -> Graph (m + p) (n + q) (ms ++ ps) (ns ++ qs)
  Before :: Graph m n ms ns -> Graph n p ns ps -> Graph m p ms ps
  Empty :: Graph 0 0 '[] '[]
  Swap :: proxy m -> proxy n -> Graph (m + n) (n + m) (ms ++ ns) (ns ++ ms)
  Feedback :: proxy m 
           -> proxy n 
           -> proxy s 
           -> Graph (m + s) (n + s) (ms ++ ss) (ns ++ ss)
           -> Graph m n ms ns
