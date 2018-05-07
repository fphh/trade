{-# LANGUAGE ScopedTypeVariables #-}


module Trade.Trade.Signal where

import Prelude hiding (div)

import qualified Data.Map as Map

import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Data.Maybe (isJust, catMaybes)

import Trade.Timeseries.Algorithm.SyncZip
import Trade.Type.EquityAndShare 

import Trade.Report.NumberedList
import Trade.Report.Pretty

import Debug.Trace

data Impulse = Buy | Sell deriving (Show, Eq)

instance Pretty Impulse where
  pretty = show

data ImpulseParameter evt = ImpulseParameter {
  eventToImpulse :: Int -> UTCTime -> evt -> Maybe Impulse
  , events :: Vector (UTCTime, evt)
  }

newtype ImpulseSignal ohcl = ImpulseSignal {
  unImpulseSignal :: Vector (UTCTime, Maybe Impulse)
  } deriving (Show)

instance ToNumberedList (ImpulseSignal ohcl) where
  toNumberedList (ImpulseSignal imps) = toNumberedList imps

toImpulseSignal ::
  (Int -> UTCTime -> evt -> Maybe Impulse)
  -> Vector (UTCTime, evt)
  -> ImpulseSignal ohcl
toImpulseSignal f vs = ImpulseSignal (Vec.imap (\i (t, x) -> (t, f i t x)) vs)

bhImpulse :: Int -> (Int -> UTCTime -> evt -> Maybe Impulse)
bhImpulse len i _ _ | i == 0 = Just Buy
bhImpulse len i _ _ | i == len-1 = Just Sell
bhImpulse _ _ _ _ = Nothing
 
bhImpulseSignal :: Vector (UTCTime, evt) -> ImpulseSignal ohcl
bhImpulseSignal vs = toImpulseSignal (bhImpulse (Vec.length vs - 1)) vs

bhImpulseParameter :: Vector (UTCTime, ohcl) -> ImpulseParameter ohcl
bhImpulseParameter vs = ImpulseParameter (bhImpulse (Vec.length vs - 1)) vs

data State =
  Long
  -- | Short
  | NoPosition
  deriving (Show)

instance Pretty State where
  pretty = show

data StateInterval = StateInterval {
  duration :: NominalDiffTime
  , state :: State
  } deriving (Show)

instance Pretty StateInterval where
  pretty (StateInterval d s) = pretty d ++ ", " ++ pretty s

data StateSignal ohlc = StateSignal {
  start :: UTCTime
  , signal :: Vector (UTCTime, StateInterval)
  } deriving (Show)

instance ToNumberedList (StateSignal ohlc) where
  toNumberedList (StateSignal start ss) = [show start] : toNumberedList ss


impulse2state :: ImpulseSignal ohlc -> StateSignal ohlc
impulse2state (ImpulseSignal is) =
  let (s, _) = Vec.head is
  
      js = Vec.fromList
           $ catMaybes
           $ Vec.toList
           $ case Vec.filter isJust (Vec.map sequence is) of
               xs | Vec.null xs -> error "impulse2state: no impulses in sequence"
               xs -> xs

      f (t0, Sell) (t1, Buy)  = (t0, StateInterval (t1 `diffUTCTime` t0) NoPosition)
      f (t0, Buy) (t1, Sell)  = (t0, StateInterval (t1 `diffUTCTime` t0) Long)
      f _ _ = error "impulse2state: unexpected sequence of impulses"

  in StateSignal s (Vec.zipWith f js (Vec.tail js))

-- buy :: (Div ohcl, Mult ohcl) => Portfolio -> ohlc -> Portfolio
buy (Portfolio eqty shrs) pps =
  let eqty' = eqty - shrs' `mult` pps
      shrs' = eqty `div` pps
  in Portfolio eqty' shrs'

-- sell :: (Div ohcl, Mult ohcl) => Portfolio -> ohlc -> Portfolio
sell (Portfolio eqty shrs) pps =
  let eqty' = eqty + shrs `mult` pps
      shrs' = 0
  in Portfolio eqty' shrs'

impulse2portfolio ::
  (Div ohlc, Mult ohlc) =>
  Portfolio -> ImpulseSignal ohlc -> PriceSignal ohlc -> PortfolioSignal
impulse2portfolio pf (ImpulseSignal is) (PriceSignal pps) =
  let (t0, _) = Vec.head pps
  
      ss = syncZip is pps

      trade (pf, acc) (t, (Nothing, _)) = (pf, (t, pf):acc)
      trade (pf, acc) (t, (Just Buy, price)) =
        let newPf = buy pf price
        in (newPf, (t, newPf):acc)
      trade (pf, acc) (t, (Just Sell, price)) =
        let newPf = sell pf price
        in (newPf, (t, newPf):acc)

  in PortfolioSignal
     $ Vec.reverse
     $ Vec.fromList
     $ snd
     $ Vec.foldl' trade (pf, [(t0, pf)]) ss

    
state2impulse :: StateSignal ohcl -> ImpulseSignal ohcl
state2impulse (StateSignal _s _vs) = error "state2impulse: to be defined"



data AbstractTrade ohcl = AbstractTrade {
  tradeTrade :: State
  , tradeDuration :: NominalDiffTime
  , enter :: ohcl
  , exit :: ohcl
  } deriving (Show)

instance (Pretty ohlc) => Pretty (AbstractTrade ohlc) where
  pretty (AbstractTrade a b c d) = pretty a ++ ", " ++ pretty b ++ ", " ++ pretty c ++ ", " ++ pretty d

data AbstractTradeSignal ohcl = AbstractTradeSignal {
  startTrades :: UTCTime
  , trades :: Vector (UTCTime, AbstractTrade ohcl)
  } deriving (Show)

instance (Pretty ohlc) => ToNumberedList (AbstractTradeSignal ohlc) where
  toNumberedList (AbstractTradeSignal s xs) = [pretty s] : (toNumberedList xs)
  
newtype PriceSignal ohcl = PriceSignal {
  unPriceSignal :: Vector (UTCTime, ohcl)
  } deriving (Show)

instance (Pretty ohlc) => ToNumberedList (PriceSignal ohlc) where
  toNumberedList (PriceSignal pps) = toNumberedList pps


state2abstractTrade :: StateSignal ohcl -> PriceSignal ohcl -> AbstractTradeSignal ohcl
state2abstractTrade (StateSignal stt vs) (PriceSignal ps) =
  let m = Map.fromList (Vec.toList ps)
      f (t, StateInterval d s) = (t, AbstractTrade s d (m Map.! t) (m Map.! (d `addUTCTime` t)))
  in AbstractTradeSignal stt (Vec.map f vs)


data Portfolio = Portfolio {
  equity :: Equity
  , shares :: Share
  } deriving (Show)

instance Pretty Portfolio where
  pretty (Portfolio eqty shrs) = pretty eqty ++ ", " ++ pretty shrs

newtype PortfolioSignal = PortfolioSignal {
  unPortfolioSignal :: Vector (UTCTime, Portfolio)
  } deriving (Show)

instance ToNumberedList PortfolioSignal where
  toNumberedList (PortfolioSignal xs) = toNumberedList xs

trade :: (Div ohcl, Mult ohcl) => (Portfolio, [(UTCTime, Portfolio)]) -> (UTCTime, AbstractTrade ohcl) -> (Portfolio, [(UTCTime, Portfolio)])
trade (pf, acc) (t, AbstractTrade Long d ent ext) =
  let pfBuy = buy pf ent
      pfSell = sell pfBuy ext
  in (pfSell, (d `addUTCTime` t, pfSell):(t, pfBuy):acc)
trade pf _ = pf


abstractTrade2portfolio :: (Mult ohcl, Div ohcl) => Portfolio -> AbstractTradeSignal ohcl -> PortfolioSignal
abstractTrade2portfolio pf (AbstractTradeSignal s ts) =
  PortfolioSignal
  $ Vec.reverse
  $ Vec.fromList
  $ snd
  $ Vec.foldl' trade (pf, []) ts


newtype EquitySignal = EquitySignal {
  unEquitySignal :: Vector (UTCTime, Equity)
  } deriving (Show)

portfolio2equity ::  (Mult ohcl) => PortfolioSignal -> PriceSignal ohcl -> EquitySignal
portfolio2equity (PortfolioSignal vs) (PriceSignal pps) =
  let ss = syncZip vs pps
      f (Portfolio eqty shrs, ps) = eqty + shrs `mult` ps
  in EquitySignal (Vec.map (fmap f) ss)

equity2curve :: EquitySignal -> Vector (UTCTime, Double)
equity2curve (EquitySignal es) = Vec.map (fmap unEquity) es


data SignalParameter evt ohlc = SignalParameter {
  portfolio :: Portfolio
  , impulseParameter :: ImpulseParameter evt
  , quotes :: PriceSignal ohlc
  }

data Signals ohlc = Signals {
  impulses :: ImpulseSignal ohlc
  , states :: StateSignal ohlc
  , abstractTrades :: AbstractTradeSignal ohlc
  , portfoliosByTrade :: PortfolioSignal
  , realPortfolios :: PortfolioSignal
  , equitiesByTrade :: EquitySignal
  , realEquities :: EquitySignal
  }

toSignals :: forall evt ohlc. (Mult ohlc, Div ohlc) => SignalParameter evt ohlc -> Signals ohlc
toSignals (SignalParameter portfolio (ImpulseParameter tradeSignal traSigInters) quotes) = 
  let impulses :: ImpulseSignal ohlc
      impulses = toImpulseSignal tradeSignal traSigInters

      states :: StateSignal ohlc
      states = impulse2state impulses

      abstractTrades :: AbstractTradeSignal ohlc
      abstractTrades = state2abstractTrade states quotes

      portfoliosByTrade :: PortfolioSignal
      portfoliosByTrade = abstractTrade2portfolio portfolio abstractTrades

      realPortfolios :: PortfolioSignal
      realPortfolios = impulse2portfolio portfolio impulses quotes

      equitiesByTrade :: EquitySignal
      equitiesByTrade = portfolio2equity portfoliosByTrade quotes

      realEquities :: EquitySignal
      realEquities = portfolio2equity realPortfolios quotes

  in Signals {
    impulses = impulses
    , states = states
    , abstractTrades = abstractTrades
    , portfoliosByTrade = portfoliosByTrade
    , realPortfolios = realPortfolios
    , equitiesByTrade = equitiesByTrade
    , realEquities = realEquities
    }
