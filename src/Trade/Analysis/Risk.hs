

module Trade.Analysis.Risk where

import qualified Data.List as List

import qualified Data.Vector as Vec

import Trade.Type.History (History(..))
import Trade.Type.Equity (Equity(..))
import Trade.Type.Broom (Broom(..))
import Trade.Type.Distribution (CDF(..))


-- | Risk
data Risk

risk :: Broom (History Equity) -> CDF Risk
risk (Broom hs) =
  let f vs =
        let l = Vec.length vs
            g i x =
              let us = Vec.filter (<=x) (Vec.slice i (l-i) vs)
              in case Vec.length us of
                   0 -> 1/0
                   _ -> Vec.maximum (Vec.map ((1-) . (/x)) us)

        in Vec.maximum (Vec.imap g vs)
        
      len = fromIntegral (length hs)

      
      qs = map (f . Vec.map (unEquity . snd) . unHistory) hs

      h i w = (i/len, w)

  in CDF (Vec.fromList (zipWith h [0..] (List.sort qs)))
