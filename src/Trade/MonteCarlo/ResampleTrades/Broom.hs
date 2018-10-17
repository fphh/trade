
module Trade.MonteCarlo.ResampleTrades.Broom where


import qualified Data.Vector as Vec

import Control.Monad (replicateM)

import Trade.Type.Yield (Yield(..))
import Trade.Type.Bars (Bars)
import Trade.Type.History (History(..))
import Trade.Type.Broom (Broom(..))
import Trade.Type.State (State(..))
import Trade.Type.NormTrade (NormTrade(..), NormTradeList(..))

import Trade.Type.Conversion.OffsettedNormTradeList2NormHistory (offsettedNormTradeList2normHistory)
  

import qualified Trade.MonteCarlo.ResampleTrades.MonteCarlo as MC


normHistoryBroom :: Bars -> Int -> NormTradeList -> IO (Broom (History Yield))
normHistoryBroom bs n ntl = do
  let soffs = MC.startingOffsets ntl
      f (NormTrade NoPosition t vs) =
        NormTrade NoPosition t (Vec.replicate (Vec.length vs + 1) (Yield 1))
      f (NormTrade state t vs) =
        NormTrade state t (Vec.cons (Yield 1) vs)

      ntl' = NormTradeList (map f (unNormTradeList ntl))
      
  offsTls <- replicateM n (MC.randomYieldSignal ntl' soffs)

  return (Broom (map (offsettedNormTradeList2normHistory bs) offsTls))

