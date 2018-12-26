{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Trade.Type.Scale where

import Trade.Type.Delta (Delta(..), CDelta)

class Scale t where
  factor :: Double -> t -> Double
  scale :: Double -> t -> t


instance Scale Double where
  factor q p = q/p
  scale q p = q*p

instance (Scale t) => Scale (CDelta t) where
  factor q (Delta dp) = factor q dp
  scale q (Delta dp) = Delta (scale q dp)
