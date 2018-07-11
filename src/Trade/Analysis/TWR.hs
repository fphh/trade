
module Trade.Analysis.TWR where

import qualified Data.Vector as Vec

import qualified Data.List as List

import Trade.Type.Equity (Equity(..))
import Trade.Type.Broom (Broom(..))
import Trade.Type.History (History(..))
import Trade.Type.Distribution (CDF(..))

-- | twr = terminal wealth relative
data TWR

terminalWealthRelative :: Equity -> Broom (History Equity) -> CDF TWR
terminalWealthRelative (Equity e) (Broom hs) =
  let twrs = List.sort (map ((/e) . unEquity . snd . Vec.last . unHistory) hs)
      len = fromIntegral (length twrs)
      g i w = (i/len, w)
  in CDF (Vec.fromList (zipWith g [0..] twrs))
