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
import Data.List (genericLength)
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

      yAxis = laxis_generate .~ (bytesAxis yAxisParams) $ setAxisColor def
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
             $ layout_title_style %~ setFontStyle
             $ layout_plots .~ (map (toPlot . mkPlot) datas)
             $ layout_title .~ (T.unpack title)
             $ layout_x_axis .~ xAxis
             $ layout_y_axis .~ yAxis
             $ layout_legend .~ legend
             $ def

showD :: Int -> String
showD x = T.unpack $ formatBytes (fromIntegral x)

bytesAxis :: (Integral i, PlotValue i) =>
                     LinearAxisParams i -> AxisFn i
bytesAxis lap ps = scaledBytesAxis lap rs ps
  where
    rs = (minimum ps,maximum ps)

scaledBytesAxis :: (Integral i, PlotValue i) =>
                 LinearAxisParams i -> (i,i) -> AxisFn i
scaledBytesAxis lap (minI,maxI) ps =
    makeAxis (_la_labelf lap) (labelvs,tickvs,gridvs)
  where
    range []  = (0,1)
    range _   | minI == maxI = (fromIntegral $ minI-1, fromIntegral $ minI+1)
              | otherwise    = (fromIntegral   minI,   fromIntegral   maxI)
--  labelvs  :: [i]
    labelvs   = stepsInt (fromIntegral $ _la_nLabels lap) r
    tickvs    = stepsInt (fromIntegral $ _la_nTicks lap)
                                  ( fromIntegral $ minimum labelvs
                                  , fromIntegral $ maximum labelvs )
    gridvs    = labelvs
    r         = range ps

stepsInt :: Integral a => a -> Range -> [a]
stepsInt nSteps range = bestSize (goodness alt0) alt0 alts
  where
    bestSize n a (a':as) = let n' = goodness a' in
                           if n' < n then bestSize n' a' as else a
    bestSize _ _ []      = []

    goodness vs          = abs (genericLength vs - nSteps)

    (alt0:alts)          = map (\n -> steps n range) sampleSteps'

    -- throw away sampleSteps that are definitely too small as
    -- they takes a long time to process                           
    sampleSteps'         = let rangeMag = ceiling (snd range - fst range)

                               (s1,s2) = span (< (rangeMag `div` nSteps)) sampleSteps
                           in ((reverse . take 5 . reverse) s1) ++ s2

    -- generate all possible step sizes
    sampleSteps          = [1,2,5, 10] ++ sampleSteps1
    sampleSteps1         = [32, 64, 128, 256, 512] ++ map (*32) sampleSteps1

    steps size (minV,maxV) = takeWhile (<b) [a,a+size..] ++ [b]
      where
        a = (floor   (minV / fromIntegral size)) * size
        b = (ceiling (maxV / fromIntegral size)) * size

