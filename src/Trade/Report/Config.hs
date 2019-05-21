

module Trade.Report.Config where

import Control.Monad.Reader (ReaderT, ask, reader)

import Graphics.Rendering.Chart.Backend.Diagrams (FontSelector, loadSansSerifFonts)

import Text.Blaze.Internal (MarkupM)

data UserConfig = UserConfig {
  chartDimension :: (Double, Double)
  }


data Config = Config {
  userCfg :: UserConfig
  , fontSelector :: FontSelector Double
  }


type HtmlReader a = ReaderT Config MarkupM a


userConfig :: HtmlReader UserConfig
userConfig = fmap userCfg ask

readUserConfig :: (UserConfig -> x) -> HtmlReader x
readUserConfig f= reader userCfg >>= return . f

defConfig :: IO Config
defConfig = fmap (Config (UserConfig (920, 560))) loadSansSerifFonts

