

module Trade.Type.Scale where



class Scale a where
  scale :: Double -> a -> a
