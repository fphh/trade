
module Trade.Example.Strategy where

import Data.Time.Clock (UTCTime)

import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Data.Vector as Vec

import qualified Data.Map as Map
import Data.Map (Map)

import Trade.Type.Conversion.Invest2Impulse (invest2impulse)
import Trade.Type.ImpulseSignal (ImpulseSignal)
import Trade.Type.Price (Price(..))
import Trade.Type.Signal (Timeseries, Signal(..))
import Trade.Type.Strategy (Long)

-- import Trade.Strategy.Library.BuyAndHold (buyAndHold)
import Trade.Strategy.Library.MovingAverages (movingAverages)

import qualified Trade.Strategy.Process as Strategy
import Trade.Strategy.Type (Window(..), AlignedSignals)

import Trade.MonteCarlo.Simulation.BlackScholes (Mu(..), Sigma(..), blackScholesDet)

import qualified Trade.Test.Data as TD
import qualified Trade.Test.Time as Time

import qualified Trade.Report.Chart as Chart
import Trade.Report.HtmlReader (render)




data Symbol = A deriving (Show, Eq, Ord)


ticker :: Timeseries Price
ticker = Signal (Vec.map (fmap Price) TD.sinus)

blackScholes :: IO (Timeseries Price)
blackScholes = do
  
  let mu = Mu 0.5
      sigma = Sigma 0.5
      start = Price 1000
      seed = 49
      vs = Time.year 2017

  blackScholesDet seed vs start mu sigma


example :: IO ()
example = do

  bs <- blackScholes

  let strategy = movingAverages (Window 10) (Window 20)

      asigs :: AlignedSignals Symbol Price
      ((asigs, stgy), _) = Strategy.run (strategy (Map.fromList [(A, bs)]))

      xs :: Map Symbol (ImpulseSignal Long)
      xs = fmap invest2impulse stgy

  t <- render (Chart.strategy xs asigs Map.empty)
  BSL.putStrLn t
  
