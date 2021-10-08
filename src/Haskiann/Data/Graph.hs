module Haskiann.Data.Graph (Graph(..)) where

import Data.Kind
import Haskiann.Data.HList

type family (++) xs ys where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

data Graph :: [Type] -> [Type] -> Type where
  Vert :: HList ms -> HList ns -> Graph ms ns
  Edge :: Graph '[k] '[k]
  Beside :: Graph ms ns 
         -> Graph ps qs 
         -> Graph (ms ++ ps) (ns ++ qs)
  Before :: Graph ms ns -> Graph ns ps -> Graph ms ps
  Empty :: Graph '[] '[]
  Swap :: Graph '[x, y] '[y, x]

