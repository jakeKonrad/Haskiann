module Haskiann.Data.TensorDesc ( TensorShape(..)
                                , TensorDesc(..)
                                ) where

import Data.Kind
import Haskiann.Data.Nat

type family (++) xs ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

data TensorShape :: [Nat] -> Type where
  Z :: TensorShape '[]
  (:.) :: !(TensorShape shape) -> !(proxy n) -> TensorShape (shape ++ (n ': '[]))
   
data TensorDesc :: k -> [Nat] -> Type where
  TensorDesc :: { dtype :: !(proxy dtype)
                , shape :: !(TensorShape shape) 
                } -> TensorDesc dtype shape 

