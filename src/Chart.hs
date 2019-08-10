
module Chart where

import Control.Lens
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.RGBSpace.HSL
import qualified Data.Text as T
import Data.Hashable
import Data.Word
import Data.Default.Class
import Graphics.Rendering.Chart

import Types
import Operations

nameColor :: T.Text -> Colour Double
nameColor name =
  let h = fromIntegral (hash name `mod` 255) :: Word8
      h' = (fromIntegral h / 255) * 360
      r = (fromIntegral h / 255)
      v = (1 - r) * 0.5 + r*0.9

      hslColor = hsl h' 0.5 v
  in  sRGB (channelRed hslColor) (channelGreen hslColor) (channelBlue hslColor)

makeChart :: SamplesData -> Layout Double Int
makeChart datas =
  let mkPlot (name, samples) =
          plot_fillbetween_style .~ solidFillStyle (opaque $ nameColor name)
          $ plot_fillbetween_title .~ (T.unpack $ niFullName $ parseName name)
          $ plot_fillbetween_values .~ samples
          $ def

  in layout_grid_last .~ True
             $ layout_plots .~ (map (toPlot . mkPlot) datas)
--              $ layout_legend .~ Nothing
             $ def

