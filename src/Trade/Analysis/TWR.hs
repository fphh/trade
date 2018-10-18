
module Trade.Analysis.TWR where

import qualified Data.Vector as Vec

import qualified Data.List as List

import Trade.Type.Broom (Broom(..))
import Trade.Type.Distribution (CDF(..))
import Trade.Type.Equity (Equity(..))
import Trade.Type.Signal (Signal(..))

import Trade.Help.SafeTail (slast)

-- | twr = terminal wealth relative
data TWR

terminalWealthRelative :: Equity -> Broom (Signal t Equity) -> CDF TWR
terminalWealthRelative (Equity e) (Broom hs) =
  let lst = slast "TWR.terminalWealthRelative" 
      twrs = List.sort (map ((/e) . unEquity . snd . lst . unSignal) hs)
      len = fromIntegral (length twrs)
      g i w = (i/len, w)
  in CDF (Vec.fromList (zipWith g [0..] twrs))
