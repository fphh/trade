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
  toUrl = show


data Symbol =
  ETHBTC
  | LTCBTC
  | BNBBTC
  | NEOBTC
  | QTUMETH
  | EOSETH
  | SNTETH
  | BNTETH
  | GASBTC
  | BNBETH
  | BTCUSDT
  | ETHUSDT
  | OAXETH
  | DNTETH
  | MCOETH
  | MCOBTC
  | WTCBTC
  | WTCETH
  | LRCBTC
  | LRCETH
  | QTUMBTC
  | YOYOBTC
  | OMGBTC
  | OMGETH
  | ZRXBTC
  | ZRXETH
  | STRATBTC
  | STRATETH
  | SNGLSBTC
  | SNGLSETH
  | BQXBTC
  | BQXETH
  | KNCBTC
  | KNCETH
  | FUNBTC
  | FUNETH
  | SNMBTC
  | SNMETH
  | NEOETH
  | IOTABTC
  | IOTAETH
  | LINKBTC
  | LINKETH
  | XVGBTC
  | XVGETH
  | SALTBTC
  | SALTETH
  | MDABTC
  | MDAETH
  | MTLBTC
  | MTLETH
  | SUBBTC
  | SUBETH
  | EOSBTC
  | SNTBTC
  | ETCETH
  | ETCBTC
  | MTHBTC
  | MTHETH
  | ENGBTC
  | ENGETH
  | DNTBTC
  | ZECBTC
  | ZECETH
  | BNTBTC
  | ASTBTC
  | ASTETH
  | DASHBTC
  | DASHETH
  | OAXBTC
  | BTGBTC
  | BTGETH
  | EVXBTC
  | EVXETH
  | REQBTC
  | REQETH
  | VIBBTC
  | VIBETH
  | TRXBTC
  | TRXETH
  | POWRBTC
  | POWRETH
  | ARKBTC
  | ARKETH
  | YOYOETH
  | XRPBTC
  | XRPETH
  | MODBTC
  | MODETH
  | ENJBTC
  | ENJETH
  | STORJBTC
  | STORJETH
  | BNBUSDT
  | YOYOBNB
  | POWRBNB
  | KMDBTC
  | KMDETH
  | NULSBNB
  | RCNBTC
  | RCNETH
  | RCNBNB
  | NULSBTC
  | NULSETH
  | RDNBTC
  | RDNETH
  | RDNBNB
  | XMRBTC
  | XMRETH
  | DLTBNB
  | WTCBNB
  | DLTBTC
  | DLTETH
  | AMBBTC
  | AMBETH
  | AMBBNB
  | BATBTC
  | BATETH
  | BATBNB
  | BCPTBTC
  | BCPTETH
  | BCPTBNB
  | ARNBTC
  | ARNETH
  | GVTBTC
  | GVTETH
  | CDTBTC
  | CDTETH
  | GXSBTC
  | GXSETH
  | NEOUSDT
  | NEOBNB
  | POEBTC
  | POEETH
  | QSPBTC
  | QSPETH
  | QSPBNB
  | BTSBTC
  | BTSETH
  | BTSBNB
  | XZCBTC
  | XZCETH
  | XZCBNB
  | LSKBTC
  | LSKETH
  | LSKBNB
  | TNTBTC
  | TNTETH
  | FUELBTC
  | FUELETH
  | MANABTC
  | MANAETH
  | BCDBTC
  | BCDETH
  | DGDBTC
  | DGDETH
  | IOTABNB
  | ADXBTC
  | ADXETH
  | ADXBNB
  | ADABTC
  | ADAETH
  | PPTBTC
  | PPTETH
  | CMTBTC
  | CMTETH
  | CMTBNB
  | XLMBTC
  | XLMETH
  | XLMBNB
  | CNDBTC
  | CNDETH
  | CNDBNB
  | LENDBTC
  | LENDETH
  | WABIBTC
  | WABIETH
  | WABIBNB
  | LTCETH
  | LTCUSDT
  | LTCBNB
  | TNBBTC
  | TNBETH
  | WAVESBTC
  | WAVESETH
  | WAVESBNB
  | GTOBTC
  | GTOETH
  | GTOBNB
  | ICXBTC
  | ICXETH
  | ICXBNB
  | OSTBTC
  | OSTETH
  | OSTBNB
  | ELFBTC
  | ELFETH
  | AIONBTC
  | AIONETH
  | AIONBNB
  | NEBLBTC
  | NEBLETH
  | NEBLBNB
  | BRDBTC
  | BRDETH
  | BRDBNB
  | MCOBNB
  | EDOBTC
  | EDOETH
  | WINGSBTC
  | WINGSETH
  | NAVBTC
  | NAVETH
  | NAVBNB
  | LUNBTC
  | LUNETH
  | APPCBTC
  | APPCETH
  | APPCBNB
  | VIBEBTC
  | VIBEETH
  | RLCBTC
  | RLCETH
  | RLCBNB
  | INSBTC
  | INSETH
  | PIVXBTC
  | PIVXETH
  | PIVXBNB
  | IOSTBTC
  | IOSTETH
  | STEEMBTC
  | STEEMETH
  | STEEMBNB
  | NANOBTC
  | NANOETH
  | NANOBNB
  | VIABTC
  | VIAETH
  | VIABNB
  | BLZBTC
  | BLZETH
  | BLZBNB
  | AEBTC
  | AEETH
  | AEBNB
  | NCASHBTC
  | NCASHETH
  | NCASHBNB
  | POABTC
  | POAETH
  | POABNB
  | ZILBTC
  | ZILETH
  | ZILBNB
  | ONTBTC
  | ONTETH
  | ONTBNB
  | STORMBTC
  | STORMETH
  | STORMBNB
  | QTUMBNB
  | QTUMUSDT
  | XEMBTC
  | XEMETH
  | XEMBNB
  | WANBTC
  | WANETH
  | WANBNB
  | WPRBTC
  | WPRETH
  | QLCBTC
  | QLCETH
  | SYSBTC
  | SYSETH
  | SYSBNB
  | QLCBNB
  | GRSBTC
  | GRSETH
  | ADAUSDT
  | ADABNB
  | CLOAKBTC
  | CLOAKETH
  | GNTBTC
  | GNTETH
  | GNTBNB
  | LOOMBTC
  | LOOMETH
  | LOOMBNB
  | XRPUSDT
  | REPBTC
  | REPETH
  | REPBNB
  | TUSDBTC
  | TUSDETH
  | TUSDBNB
  | ZENBTC
  | ZENETH
  | ZENBNB
  | SKYBTC
  | SKYETH
  | SKYBNB
  | EOSUSDT
  | EOSBNB
  | CVCBTC
  | CVCETH
  | CVCBNB
  | THETABTC
  | THETAETH
  | THETABNB
  | XRPBNB
  | TUSDUSDT
  | IOTAUSDT
  | XLMUSDT
  | IOTXBTC
  | IOTXETH
  | QKCBTC
  | QKCETH
  | AGIBTC
  | AGIETH
  | AGIBNB
  | NXSBTC
  | NXSETH
  | NXSBNB
  | ENJBNB
  | DATABTC
  | DATAETH
  | ONTUSDT
  | TRXBNB
  | TRXUSDT
  | ETCUSDT
  | ETCBNB
  | ICXUSDT
  | SCBTC
  | SCETH
  | SCBNB
  | NPXSBTC
  | NPXSETH
  | KEYBTC
  | KEYETH
  | NASBTC
  | NASETH
  | NASBNB
  | MFTBTC
  | MFTETH
  | MFTBNB
  | DENTBTC
  | DENTETH
  | ARDRBTC
  | ARDRETH
  | ARDRBNB
  | NULSUSDT
  | HOTBTC
  | HOTETH
  | VETBTC
  | VETETH
  | VETUSDT
  | VETBNB
  | DOCKBTC
  | DOCKETH
  | POLYBTC
  | POLYBNB
  | PHXBTC
  | PHXETH
  | PHXBNB
  | HCBTC
  | HCETH
  | GOBTC
  | GOBNB
  | PAXUSDT
  | RVNBTC
  | RVNBNB
  | DCRBTC
  | DCRBNB
  | MITHBTC
  | MITHBNB
  | BCHABCBTC
  | BCHSVBTC
  | BCHABCUSDT
  | BCHSVUSDT
  | BNBPAX
  | BTCPAX
  | ETHPAX
  | XRPPAX
  | EOSPAX
  | XLMPAX
  | RENBTC
  | RENBNB
  | XRPTUSD
  | EOSTUSD
  | XLMTUSD
  | BNBUSDC
  | BTCUSDC
  | ETHUSDC
  | XRPUSDC
  | EOSUSDC
  | XLMUSDC
  | USDCUSDT
  | ADATUSD
  | TRXTUSD
  | NEOTUSD
  | TRXXRP
  | XZCXRP
  | PAXTUSD
  | USDCTUSD
  | USDCPAX
  | LINKUSDT
  | LINKTUSD
  | LINKPAX
  | LINKUSDC
  | WAVESUSDT
  | WAVESTUSD
  | WAVESPAX
  | WAVESUSDC
  | BCHABCTUSD
  | BCHABCPAX
  | BCHABCUSDC
  | BCHSVTUSD
  | BCHSVPAX
  | BCHSVUSDC
  | LTCTUSD
  | LTCPAX
  | LTCUSDC
  | TRXPAX
  | TRXUSDC
  | BTTBTC
  | BTTBNB
  | BTTUSDT
  | BNBUSDS
  | BTCUSDS
  | USDSUSDT
  | USDSPAX
  | USDSTUSD
  | USDSUSDC
  | BTTPAX
  | BTTTUSD
  | BTTUSDC
  | ONGBNB
  | ONGBTC
  | ONGUSDT
  | HOTBNB
  | HOTUSDT
  | ZILUSDT
  deriving (Show, Read)
