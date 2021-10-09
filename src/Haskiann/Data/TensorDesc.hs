module Haskiann.Data.TensorDesc ( TensorShape(..)
                                , Fixed
                                , Dynamic
                                , TensorDesc(..)
                                ) where

import Data.Kind
import Haskiann.Data.Nat

type family (++) xs ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

data TensorShape :: [Nat] -> Type where
  Scalar :: TensorShape '[]
  (:&) :: !(TensorShape shape) -> !(proxy n) -> TensorShape (shape ++ '[n])
   
data Fixed :: k -> [Nat] -> Type

data Dynamic :: Type

data TensorDesc a where
  Fixed :: !(proxy dtype) -> !(TensorShape shape) -> TensorDesc (Fixed dtype shape)
  Dynamic :: dtype -> [Natural] -> TensorDesc Dynamic

