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
import qualified GI.Gdk

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

makeChart :: ChartData -> Layout Double Int
makeChart chart =
  let title = chtTitle chart
      mbHighlight = chtHighlgiht chart
      datas = chtSamples chart

      mkPlot (name, samples) =
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

      setFontStyle t =
        case chtTheme chart of
          Nothing -> t
          Just theme -> font_color .~ thmForeground theme $ t

      setLineColor =
        case chtTheme chart of
          Nothing -> id
          Just theme -> line_color .~ thmForeground theme

      gridStyle =
        case chtTheme chart of
          Nothing -> defaultGridLineStyle
          Just theme -> dashedLine 1 [5, 5] $ dissolve 0.5 (thmForeground theme)

      setAxisColor =
          laxis_style %~ (axis_label_style %~ setFontStyle) .
                         (axis_line_style %~ setLineColor) .
                         (axis_grid_style .~ gridStyle)

      yAxis = laxis_generate .~ (autoScaledIntAxis yAxisParams) $ setAxisColor def
      yAxisParams = la_labelf .~ (map showD) $ (defaultIntAxis :: LinearAxisParams Int)

      xAxis = setAxisColor def

      setLegendFont =
        case chtTheme chart of
          Nothing -> id
          Just theme -> legend_label_style %~ setFontStyle

      legend =
        if chtLegend chart
          then Just $ setLegendFont $ legend_orientation .~ LOCols 4 $ def
          else Nothing

      background =
        case chtTheme chart of
          Nothing -> id
          Just theme -> fill_color .~ transparent

  in layout_grid_last .~ True
             $ layout_background %~ background
             $ layout_plots .~ (map (toPlot . mkPlot) datas)
             $ layout_title .~ (T.unpack title)
             $ layout_x_axis .~ xAxis
             $ layout_y_axis .~ yAxis
             $ layout_legend .~ legend
             $ def

showD :: Int -> String
showD x = T.unpack $ formatNum bytesFormat (fromIntegral x)

