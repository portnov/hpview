{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Numeric (showEFloat, showFFloat)
import Formattable.NumFormat

import Types
import Operations

nameColor :: T.Text -> Colour Double
nameColor name =
  let h = fromIntegral (hash name `mod` 255) :: Word8
      hue = fromIntegral (hash name `mod` 360)
      r = (fromIntegral h / 255)
      v = (1 - r) * 0.5 + r*0.9

      hslColor = hsl hue 0.5 v
  in  sRGB (channelRed hslColor) (channelGreen hslColor) (channelBlue hslColor)

makeChart :: T.Text -> Maybe T.Text -> SamplesData -> Layout Double Int
makeChart title mbHighlight datas =
  let mkPlot (name, samples) =
          plot_fillbetween_style .~ solidFillStyle (fillColor name)
          $ plot_fillbetween_line .~ Just (solidLine (lineWidth name) (lineColor name))
          $ plot_fillbetween_title .~ (T.unpack $ niFullName $ parseName name)
          $ plot_fillbetween_values .~ samples
          $ def

      fillColor name = opaque $ nameColor name

      lineWidth name
        | mbHighlight == Just name = 2
        | otherwise = 1

      lineColor name
        | mbHighlight == Just name = darken 0.1 (fillColor name)
        | otherwise = darken 0.5 (fillColor name)

      yAxis = laxis_generate .~ (autoScaledIntAxis yAxisParams) $ def

      yAxisParams = la_labelf .~ (map showD) $ (defaultIntAxis :: LinearAxisParams Int)

      legend = legend_orientation .~ LOCols 4 $ def

  in layout_grid_last .~ True
             $ layout_plots .~ (map (toPlot . mkPlot) datas)
             $ layout_title .~ (T.unpack title)
             $ layout_y_axis .~ yAxis
             $ layout_legend .~ Just legend
--              $ layout_legend .~ Nothing
             $ def

showD :: Int -> String
showD x = T.unpack $ formatNum bytesFormat (fromIntegral x)

