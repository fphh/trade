

module Trade.Type.Conversion.Type2Double where



class Type2Double t where
  type2double :: t -> Double

instance Type2Double Double where
  type2double = id
