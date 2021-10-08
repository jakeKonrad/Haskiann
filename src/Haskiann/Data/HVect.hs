module Haskiann.Data.HVect where

import Data.Kind
import Haskiann.Data.Nat 

infixr 8 :::

data HVect :: [Type] -> Nat -> Type where
  Nil :: HVect '[] 0
  (:::) :: k -> HVect ks n -> HVect (k ': ks) (n + 1)

