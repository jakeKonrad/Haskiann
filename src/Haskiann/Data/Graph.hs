{-# LANGUAGE UndecidableInstances #-}
module Haskiann.Data.Graph (Graph(..)) where

import Data.Kind
import Haskiann.Data.HList
import Haskiann.Data.TensorDesc (TensorDesc)
import GHC.TypeLits

type family (++) xs ys where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family GraphConstraint xs :: Constraint where
  GraphConstraint '[] = ()
  GraphConstraint (TensorDesc _ _ ': xs) = GraphConstraint xs
  GraphConstraint (t ': _) = TypeError ('Text "Expected a TensorDesc." ':$$: 
                                        'Text "Got a " ':<>: 'ShowType t ':<>: 'Text " instead.")

data Graph :: [Type] -> [Type] -> Type where
  Vert :: (GraphConstraint ms, GraphConstraint ns) 
       => HList ms 
       -> HList ns 
       -> Graph ms ns
  Edge :: GraphConstraint '[k] => Graph '[k] '[k]
  Beside :: (GraphConstraint ms, GraphConstraint ns, GraphConstraint ps, GraphConstraint qs)
         => Graph ms ns 
         -> Graph ps qs 
         -> Graph (ms ++ ps) (ns ++ qs)
  Before :: (GraphConstraint ms, GraphConstraint ns, GraphConstraint ps)
         => Graph ms ns 
         -> Graph ns ps 
         -> Graph ms ps
  Empty :: Graph '[] '[]
  Swap :: (GraphConstraint '[x, y], GraphConstraint '[y, x]) 
       => Graph '[x, y] '[y, x]

