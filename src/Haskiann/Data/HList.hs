module Haskiann.Data.HList where

import Data.Kind

infixr 8 :::

data HList :: [Type] -> Type where
  Nil :: HList '[]
  (:::) :: k -> HList ks -> HList (k ': ks)

