{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Trade.Trade.Signal where

import Prelude hiding (div)

import qualified Data.Map as Map

import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)

import qualified Data.Vector as Vec
import Data.Vector (Vector)

import qualified Data.List as List


import Data.Function (on)

import Trade.Timeseries.Algorithm.SyncZip
import Trade.Type.EquityAndShare 
import Trade.Type.Yield 

import Trade.Report.NumberedList
import Trade.Report.Pretty

import Trade.Trade.ImpulseSignal
import Trade.Trade.PriceSignal
import Trade.Trade.StateSignal
import Trade.Trade.State
import Trade.Trade.Curve



buy :: (Div ohcl, Mult ohcl) => Portfolio -> ohcl -> Portfolio
buy (Portfolio eqty shrs) pps =
  let eqty' = eqty - shrs' `mult` pps
      shrs' = eqty `div` pps
  in Portfolio eqty' shrs'

sell :: (Mult ohlc) => Portfolio -> ohlc -> Portfolio
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


state2abstractTrade :: StateSignal ohcl -> PriceSignal ohcl -> AbstractTradeSignal ohcl
state2abstractTrade (StateSignal stt vs) (PriceSignal ps) =
  let m = Map.fromList (Vec.toList ps)
      f (t, StateInterval d s) = (t, AbstractTrade s d (m Map.! t) (m Map.! (d `addUTCTime` t)))
  in AbstractTradeSignal stt (Vec.map f vs)

instance Curve (Vector (UTCTime, Yield)) where
  curve vs = Vec.map (fmap unYield) vs

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


instance Curve EquitySignal where
  curve (EquitySignal es) = Vec.map (fmap unEquity) es

instance Curve DrawdownSignal where
 curve (DrawdownSignal ds) = Vec.map (fmap toAbsDD) ds

instance Curve AccDrawdownSignal where
 curve (AccDrawdownSignal start ds) =
   let vs = Vec.toList ds
       (tl, AccDrawdown d yl) = Vec.last ds
       f (t0, AccDrawdown _ y0) (t1, AccDrawdown _ y1) =
         [(t0, toAbsDD y0), (t1, toAbsDD y0), (t1, toAbsDD y1)]
   in Vec.fromList (concat (zipWith f vs (tail vs)) ++ [(d `addUTCTime` tl, toAbsDD yl)])
   


data Drawdown = Drawdown {
  drawdownIn :: Equity
  , drawdownOut :: Equity
  } deriving (Show)



toAbsoluteDrawdown :: Drawdown -> Equity
toAbsoluteDrawdown (Drawdown i o) = i-o

toAbsDD :: Drawdown -> Double
toAbsDD = unEquity . toAbsoluteDrawdown

maxAbsoluteDrawdown :: DrawdownSignal -> Drawdown
maxAbsoluteDrawdown (DrawdownSignal ds) = snd (Vec.maximumBy (compare `on` (toAbsDD . snd)) ds)

toRelDD :: Drawdown -> Double
toRelDD (Drawdown (Equity i) (Equity o)) = 1-o/i

maxRelativeDrawdown :: DrawdownSignal -> Drawdown
maxRelativeDrawdown (DrawdownSignal ds) = snd (Vec.maximumBy (compare `on` (toRelDD . snd)) ds)


instance Pretty Drawdown where
  pretty = show

newtype DrawdownSignal = DrawdownSignal {
  unDrawdownSignal :: Vector (UTCTime, Drawdown)
  } deriving (Show)

realEquity2drawdown :: EquitySignal -> DrawdownSignal
realEquity2drawdown (EquitySignal es) =
  let len = Vec.length es
      f i (t, u) = (t, Drawdown u (Vec.minimum (Vec.map snd (Vec.slice i (len-i) es))))
  in DrawdownSignal (Vec.imap f es)

data AccDrawdown = AccDrawdown {
  ddDuration :: NominalDiffTime
  , accDD :: Drawdown
  } deriving (Show)

instance Pretty AccDrawdown where
  pretty (AccDrawdown a b) = show a ++ "|" ++ show b

data AccDrawdownSignal = AccDrawdownSignal {
  ddStart :: UTCTime
  , unAccDrawdownSignal :: Vector (UTCTime, AccDrawdown)
  } deriving (Show)

accumulateDrawdown :: DrawdownSignal -> AccDrawdownSignal
accumulateDrawdown (DrawdownSignal ds) =
  let start =
        case ds Vec.!? 0 of
          Nothing -> error "accumulateDrawdown: empty signal"
          Just (s, _) -> s

      (ts, ys) = Vec.unzip ds
      ys' = Vec.toList ys

      g xs =
        let (t0, y0) = head xs
            (t1, _) = last xs
        in (t0, AccDrawdown (t1 `diffUTCTime` t0) y0)

      h (mx, acc) y = if toAbsDD mx < toAbsDD y then (y, y:acc) else (mx, mx:acc)
        
      as =
        map g
        $ List.groupBy (\a b -> toAbsDD (snd a) == toAbsDD (snd b))
        $ zip (Vec.toList ts) (reverse $ snd $ List.foldl' h (Drawdown 0 0, []) ys')
      
  in AccDrawdownSignal start (Vec.fromList as)



data SignalParameter evt ohlc = SignalParameter {
  portfolio :: Portfolio
  , impulseParameter :: ImpulseParameter evt
  , quotesParameter :: PriceSignal ohlc
  }

data Signals ohlc = Signals {
  quotes :: PriceSignal ohlc
  , impulses :: ImpulseSignal ohlc
  , states :: StateSignal ohlc
  , abstractTrades :: AbstractTradeSignal ohlc
  -- , yields :: YieldSignal ohlc
  , portfoliosByTrade :: PortfolioSignal
  , realPortfolios :: PortfolioSignal
  , equitiesByTrade :: EquitySignal
  , realEquities :: EquitySignal
  , drawdown :: DrawdownSignal
  , accDrawdown :: AccDrawdownSignal
  }

toSignals :: forall evt ohlc. (Mult ohlc, Div ohlc, ToYield ohlc) => SignalParameter evt ohlc -> Signals ohlc
toSignals (SignalParameter portfolio (ImpulseParameter tradeSignal traSigInters) quotes) = 
  let impulses :: ImpulseSignal ohlc
      impulses = toImpulseSignal tradeSignal traSigInters

      states :: StateSignal ohlc
      states = impulse2state impulses

      abstractTrades :: AbstractTradeSignal ohlc
      abstractTrades = state2abstractTrade states quotes

      -- yields :: YieldSignal ohlc
      -- yields = abstractTrade2yield abstractTrades

      portfoliosByTrade :: PortfolioSignal
      portfoliosByTrade = abstractTrade2portfolio portfolio abstractTrades

      realPortfolios :: PortfolioSignal
      realPortfolios = impulse2portfolio portfolio impulses quotes

      equitiesByTrade :: EquitySignal
      equitiesByTrade = portfolio2equity portfoliosByTrade quotes

      realEquities :: EquitySignal
      realEquities  = portfolio2equity realPortfolios quotes

      drawdown :: DrawdownSignal
      drawdown = realEquity2drawdown realEquities

      accDrawdown :: AccDrawdownSignal
      accDrawdown = accumulateDrawdown drawdown

  in Signals {
    quotes = quotes
    , impulses = impulses
    , states = states
    , abstractTrades = abstractTrades
    -- , yields = yields
    , portfoliosByTrade = portfoliosByTrade
    , realPortfolios = realPortfolios
    , equitiesByTrade = equitiesByTrade
    , realEquities = realEquities
    , drawdown = drawdown
    , accDrawdown = accDrawdown
    }



{-

data TradeYield ohcl = TradeYield {
  yieldTrade :: State
  , yieldDuration :: NominalDiffTime
  , yield :: Yield
  } deriving (Show)

newtype YieldSignal ohlc = YieldSignal {
  unYieldSignal :: Vector (UTCTime, TradeYield ohlc)
  } deriving (Show)

abstractTrade2yield :: (ToYield ohlc) => AbstractTradeSignal ohlc -> YieldSignal ohlc
abstractTrade2yield (AbstractTradeSignal _ ts) =
  let toYield (AbstractTrade s d ent ext) = TradeYield s d (forwardYield ent ext)
  in YieldSignal (Vec.map (fmap toYield) ts)

newtype Fraction = Fraction {
  unFraction :: Double
  } deriving (Show)

yield2equity :: Fraction -> YieldSignal ohlc -> EquitySignal
yield2equity (Fraction f) (YieldSignal ys) =
  let g acc (t, TradeYield NoPosition _ _) = acc
      g (Equity eqty) (t, TradeYield Long _ (Yield y)) =
        let a = eqty * f
            b = eqty * (1-f)
        in Equity (a*y + b)
      ts = Vec.map fst ys
  in EquitySignal (Vec.zip ts (Vec.scanl' g (Equity 1) ys))

instance Curve (YieldSignal ohlc) where
 curve (YieldSignal ys) = Vec.map (fmap (unYield . yield)) ys
-}
