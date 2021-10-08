module Haskiann.Data.Tensor ( TensorShape(..)
                            , Tensor(..)
                            ) where

import Data.Kind
import Haskiann.Data.Nat

type family (++) xs ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

data TensorShape :: [Nat] -> Type where
  Z :: TensorShape '[]
  (:.) :: !(TensorShape shape) -> !(proxy n) -> TensorShape (shape ++ (n ': '[]))
   
data Tensor :: k -> [Nat] -> Type where
  Tensor :: { dtype :: !(proxy dtype)
            , shape :: !(TensorShape shape) 
            } -> Tensor dtype shape 

