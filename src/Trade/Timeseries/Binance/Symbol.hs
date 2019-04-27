{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Trade.Timeseries.Binance.Symbol where

import GHC.Generics

import qualified Data.Text as Text

import qualified Data.Vector as Vec

import qualified Data.Aeson as Aeson

import Data.Scientific

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX

import Trade.Timeseries.Url
import Trade.Timeseries.Row

import Trade.Type.OHLC (Open(..), High(..), Low(..), Close(..), Volume(..))


data Row = Row {
  fromDate :: !UTCTime
  , toDate :: !UTCTime
  , open :: !Open
  , high :: !High
  , low :: !Low
  , close :: !Close
  , volume :: !Volume
  } deriving (Show, Generic)

millisToUTC :: Integer -> UTCTime
millisToUTC t = posixSecondsToUTCTime $ (fromInteger t) / 1000

instance Aeson.FromJSON Row where
  parseJSON (Aeson.Array as) =
    let Aeson.Number from2 = as Vec.! 0
        Aeson.String o = as Vec.! 1
        Aeson.String h = as Vec.! 2
        Aeson.String l = as Vec.! 3
        Aeson.String c = as Vec.! 4
        Aeson.String v = as Vec.! 5
        Aeson.Number to2 = as Vec.! 6
    in pure $ Row {
      fromDate = either (error "Binance.Symbol: from") millisToUTC (floatingOrInteger from2 :: Either Double Integer)
      , toDate = either (error "Binance.Symbol: to") millisToUTC (floatingOrInteger to2 :: Either Double Integer)
      , open = Open (read $ Text.unpack o)
      , high = High (read $ Text.unpack h)
      , low = Low (read $ Text.unpack l)
      , close = Close (read $ Text.unpack c)
      , volume = Volume (read $ Text.unpack v)
      }
  parseJSON _ = error "Binance.Symbol: FromJSON not an array"

instance RowInterface Row where
  dateR = toDate
  openR = Just . open
  highR = Just . high
  lowR = Just . low
  closeR = Just . close
  volumeR = Just . volume

instance DateInterface Row where
  type TyD Row = UTCTime
  type TyR Row = Row
  dateDI = toDate
  removeDI = id

instance ToUrl Symbol where
  toUrl sym =
    case sym of
      BNB s -> show s
      BTC s -> show s
      ETH s -> show s
      PAX s -> show s
      USDC s -> show s
      USDS s -> show s
      USDT s -> show s
      XRP s -> show s

data Symbol =
  BNB BNBPair
  | BTC BTCPair
  | ETH ETHPair
  | PAX PAXPair
  | USDC USDCPair
  | USDS USDSPair
  | USDT USDTPair
  | XRP XRPPair
  deriving (Show, Eq, Ord)



data BNBPair =
  ADABNB
  | ADXBNB
  | AEBNB
  | AGIBNB
  | AIONBNB
  | AMBBNB
  | APPCBNB
  | ARDRBNB
  | BATBNB
  | BCPTBNB
  | BLZBNB
  | BRDBNB
  | BTSBNB
  | BTTBNB
  | CMTBNB
  | CNDBNB
  | CVCBNB
  | DCRBNB
  | DLTBNB
  | ENJBNB
  | EOSBNB
  | ETCBNB
  | GNTBNB
  | GOBNB
  | GTOBNB
  | HOTBNB
  | ICXBNB
  | IOTABNB
  | LOOMBNB
  | LSKBNB
  | LTCBNB
  | MCOBNB
  | MFTBNB
  | MITHBNB
  | NANOBNB
  | NASBNB
  | NAVBNB
  | NCASHBNB
  | NEBLBNB
  | NEOBNB
  | NULSBNB
  | NXSBNB
  | ONGBNB
  | ONTBNB
  | OSTBNB
  | PHXBNB
  | PIVXBNB
  | POABNB
  | POLYBNB
  | POWRBNB
  | QLCBNB
  | QSPBNB
  | QTUMBNB
  | RCNBNB
  | RDNBNB
  | RENBNB
  | REPBNB
  | RLCBNB
  | RVNBNB
  | SCBNB
  | SKYBNB
  | STEEMBNB
  | STORMBNB
  | SYSBNB
  | THETABNB
  | TRXBNB
  | VETBNB
  | VIABNB
  | WABIBNB
  | WANBNB
  | WAVESBNB
  | WTCBNB
  | XEMBNB
  | XLMBNB
  | XRPBNB
  | XZCBNB
  | YOYOBNB
  | ZENBNB
  | ZILBNB
  deriving (Show, Read, Eq, Ord, Enum)

data BTCPair =
  ADABTC
  | ADXBTC
  | AEBTC
  | AGIBTC
  | AIONBTC
  | AMBBTC
  | APPCBTC
  | ARDRBTC
  | ARKBTC
  | ARNBTC
  | ASTBTC
  | BATBTC
  | BCDBTC
  | BCHABCBTC
  | BCHSVBTC
  | BCPTBTC
  | BLZBTC
  | BNBBTC
  | BNTBTC
  | BQXBTC
  | BRDBTC
  | BTGBTC
  | BTSBTC
  | BTTBTC
  | CDTBTC
  | CMTBTC
  | CNDBTC
  | CVCBTC
  | DASHBTC
  | DATABTC
  | DCRBTC
  | DENTBTC
  | DGDBTC
  | DLTBTC
  | DNTBTC
  | DOCKBTC
  | EDOBTC
  | ELFBTC
  | ENGBTC
  | ENJBTC
  | EOSBTC
  | ETCBTC
  | ETHBTC
  | EVXBTC
  | FUELBTC
  | FUNBTC
  | GASBTC
  | GNTBTC
  | GOBTC
  | GRSBTC
  | GTOBTC
  | GVTBTC
  | GXSBTC
  | HCBTC
  | HOTBTC
  | ICXBTC
  | INSBTC
  | IOSTBTC
  | IOTABTC
  | IOTXBTC
  | KEYBTC
  | KMDBTC
  | KNCBTC
  | LENDBTC
  | LINKBTC
  | LOOMBTC
  | LRCBTC
  | LSKBTC
  | LTCBTC
  | LUNBTC
  | MANABTC
  | MCOBTC
  | MDABTC
  | MFTBTC
  | MITHBTC
  | MTHBTC
  | MTLBTC
  | NANOBTC
  | NASBTC
  | NAVBTC
  | NCASHBTC
  | NEBLBTC
  | NEOBTC
  | NPXSBTC
  | NULSBTC
  | NXSBTC
  | OAXBTC
  | OMGBTC
  | ONGBTC
  | ONTBTC
  | OSTBTC
  | PHXBTC
  | PIVXBTC
  | POABTC
  | POEBTC
  | POLYBTC
  | POWRBTC
  | PPTBTC
  | QKCBTC
  | QLCBTC
  | QSPBTC
  | QTUMBTC
  | RCNBTC
  | RDNBTC
  | RENBTC
  | REPBTC
  | REQBTC
  | RLCBTC
  | RVNBTC
  | SCBTC
  | SKYBTC
  | SNGLSBTC
  | SNMBTC
  | SNTBTC
  | STEEMBTC
  | STORJBTC
  | STORMBTC
  | STRATBTC
  | SYSBTC
  | THETABTC
  | TNBBTC
  | TNTBTC
  | TRXBTC
  | VETBTC
  | VIABTC
  | VIBBTC
  | VIBEBTC
  | WABIBTC
  | WANBTC
  | WAVESBTC
  | WPRBTC
  | WTCBTC
  | XEMBTC
  | XLMBTC
  | XMRBTC
  | XRPBTC
  | XVGBTC
  | XZCBTC
  | YOYOBTC
  | ZECBTC
  | ZENBTC
  | ZILBTC
  | ZRXBTC
  deriving (Show, Read, Eq, Ord, Enum)

data ETHPair =
  ADAETH
  | ADXETH
  | AEETH
  | AGIETH
  | AIONETH
  | AMBETH
  | APPCETH
  | ARDRETH
  | ARKETH
  | ARNETH
  | ASTETH
  | BATETH
  | BCDETH
  | BCPTETH
  | BLZETH
  | BNBETH
  | BNTETH
  | BQXETH
  | BRDETH
  | BTGETH
  | BTSETH
  | CDTETH
  | CMTETH
  | CNDETH
  | CVCETH
  | DASHETH
  | DATAETH
  | DENTETH
  | DGDETH
  | DLTETH
  | DNTETH
  | DOCKETH
  | EDOETH
  | ELFETH
  | ENGETH
  | ENJETH
  | EOSETH
  | ETCETH
  | EVXETH
  | FUELETH
  | FUNETH
  | GNTETH
  | GRSETH
  | GTOETH
  | GVTETH
  | GXSETH
  | HCETH
  | HOTETH
  | ICXETH
  | INSETH
  | IOSTETH
  | IOTAETH
  | IOTXETH
  | KEYETH
  | KMDETH
  | KNCETH
  | LENDETH
  | LINKETH
  | LOOMETH
  | LRCETH
  | LSKETH
  | LTCETH
  | LUNETH
  | MANAETH
  | MCOETH
  | MDAETH
  | MFTETH
  | MTHETH
  | MTLETH
  | NANOETH
  | NASETH
  | NAVETH
  | NCASHETH
  | NEBLETH
  | NEOETH
  | NPXSETH
  | NULSETH
  | NXSETH
  | OAXETH
  | OMGETH
  | ONTETH
  | OSTETH
  | PHXETH
  | PIVXETH
  | POAETH
  | POEETH
  | POWRETH
  | PPTETH
  | QKCETH
  | QLCETH
  | QSPETH
  | QTUMETH
  | RCNETH
  | RDNETH
  | REPETH
  | REQETH
  | RLCETH
  | SCETH
  | SKYETH
  | SNGLSETH
  | SNMETH
  | SNTETH
  | STEEMETH
  | STORJETH
  | STORMETH
  | STRATETH
  | SYSETH
  | THETAETH
  | TNBETH
  | TNTETH
  | TRXETH
  | VETETH
  | VIAETH
  | VIBEETH
  | VIBETH
  | WABIETH
  | WANETH
  | WAVESETH
  | WPRETH
  | WTCETH
  | XEMETH
  | XLMETH
  | XMRETH
  | XRPETH
  | XVGETH
  | XZCETH
  | YOYOETH
  | ZECETH
  | ZENETH
  | ZILETH
  | ZRXETH
  deriving (Show, Read, Eq, Ord, Enum)

data PAXPair =
  BCHABCPAX
  | BCHSVPAX
  | BNBPAX
  | BTCPAX
  | BTTPAX
  | EOSPAX
  | ETHPAX
  | LINKPAX
  | LTCPAX
  | TRXPAX
  | USDCPAX
  | USDSPAX
  | WAVESPAX
  | XLMPAX
  | XRPPAX
  deriving (Show, Read, Eq, Ord, Enum)

data USDPair =
  ADATUSD
  | BCHABCTUSD
  | BCHSVTUSD
  | BNBTUSD
  | BTCTUSD
  | BTTTUSD
  | EOSTUSD
  | ETHTUSD
  | LINKTUSD
  | LTCTUSD
  | NEOTUSD
  | PAXTUSD
  | TRXTUSD
  | USDCTUSD
  | USDSTUSD
  | WAVESTUSD
  | XLMTUSD
  | XRPTUSD
  deriving (Show, Read, Eq, Ord, Enum)

data USDCPair =
  BCHABCUSDC
  | BCHSVUSDC
  | BNBUSDC
  | BTCUSDC
  | BTTUSDC
  | EOSUSDC
  | ETHUSDC
  | LINKUSDC
  | LTCUSDC
  | TRXUSDC
  | USDSUSDC
  | WAVESUSDC
  | XLMUSDC
  | XRPUSDC
  deriving (Show, Read, Eq, Ord, Enum)

data USDSPair =
  BNBUSDS
  | BTCUSDS
  deriving (Show, Read, Eq, Ord, Enum)

data USDTPair =
  ADAUSDT
  | BCHABCUSDT
  | BCHSVUSDT
  | BNBUSDT
  | BTCUSDT
  | BTTUSDT
  | EOSUSDT
  | ETCUSDT
  | ETHUSDT
  | HOTUSDT
  | ICXUSDT
  | IOTAUSDT
  | LINKUSDT
  | LTCUSDT
  | NEOUSDT
  | NULSUSDT
  | ONGUSDT
  | ONTUSDT
  | PAXUSDT
  | QTUMUSDT
  | TRXUSDT
  | TUSDUSDT
  | USDCUSDT
  | USDSUSDT
  | VETUSDT
  | WAVESUSDT
  | XLMUSDT
  | XRPUSDT
  | ZILUSDT
  deriving (Show, Read, Eq, Ord, Enum)

data XRPPair =
  TRXXRP
  | XZCXRP
  deriving (Show, Read, Eq, Ord, Enum)

