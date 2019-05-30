{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Trade.Type.Strategy.Index where



newtype Index = Index {
  unIndex :: Int
  } deriving (Eq, Ord, Show, Num, Enum, Real, Integral)
