{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Gui where

import Control.Monad
import Control.Lens
import Data.Default.Class
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Int
import Data.IORef
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Format.Heavy (format)
import qualified GI.Gtk as GI (main, init)
import qualified GI.Gdk
import GI.Gdk.Structs
import GI.Gtk hiding (main)
import qualified GI.Cairo
import Graphics.Rendering.Chart as Chart
import Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Chart.Backend.Cairo
import Formattable.NumFormat

import Types
import Chart
import Config
import SettingsDlg
import Operations
import GiCairoBridge

dfltTracePercent :: Int
dfltTracePercent = 1

type LayoutPickFn = PickFn (LayoutPick Double Int Int)
type ChartCache = M.Map (Maybe T.Text) (Surface, LayoutPickFn)

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = forM_

runWindow :: Heap -> IO ()
runWindow heap = do
  initCfg <- loadConfig
  cfgRef <- newIORef initCfg

  GI.init Nothing
  -- Create a new window
  window <- windowNew WindowTypeToplevel
  -- Here we connect the "destroy" event to a signal handler.
  onWidgetDestroy window mainQuit

  highlightRef <- newIORef Nothing
  pointerRef <- newIORef Nothing
  chartSurfaceRef <- newIORef M.empty

  let invalidateChart = writeIORef chartSurfaceRef M.empty

  nSamples <- askConfig cfgSamplesNr cfgRef
  let allDatas = allSamplesData $ filterHeap Nothing 10 TraceTotal dfltTracePercent (const True) True $ resampleHeap nSamples heap
  dataRef <- newIORef allDatas

  fromXRef <- newIORef Nothing
  selectionRef <- newIORef Nothing
  timeFilterRef <- newIORef []
  
  let title = hJob (heapHeader heap) <> " at " <> hDate (heapHeader heap)

  searchHbox <- boxNew OrientationHorizontal 0
  entry <- searchEntryNew
  maxItemsLbl <- labelNew (Just "Max. items:")
  searchButton <- buttonNewWithLabel "Filter"
  maxSpin <- spinButtonNewWithRange 0 100 1
  spinButtonSetValue maxSpin 10

  tracePercentLbl <- labelNew (Just "%")
  tracePercentSpin <- spinButtonNewWithRange 1 100 1
  spinButtonSetValue tracePercentSpin (fromIntegral dfltTracePercent)

  searchFieldCombo <- mkComboBox [
                           (Name, "Name")
                         , (Module, "Module")
                         , (Package, "Package")
                       ]

  searchMethodCombo <- mkComboBox [
                           (Contains, "Contains")
                         , (Exact, "Exact")
                         , (Regexp, "Reg.Exp")
                       ]
  
  drawTraceCheckbox <- checkButtonNewWithLabel "Show trace elements"
  toggleButtonSetActive drawTraceCheckbox True

  traceStyleCombo <- mkComboBox [
                           (TraceTotal, "All trace elements give in total less than...")
                         , (TraceEach, "Each trace element gives less than...")
                       ]
  
  boxPackStart searchHbox searchFieldCombo False False 0
  boxPackStart searchHbox searchMethodCombo False False 0
  boxPackStart searchHbox entry True True 0
  boxPackStart searchHbox maxItemsLbl False False 10
  boxPackStart searchHbox maxSpin False False 10
  boxPackStart searchHbox traceStyleCombo False False 0
  boxPackStart searchHbox tracePercentSpin False False 0
  boxPackStart searchHbox tracePercentLbl False False 0
  boxPackStart searchHbox drawTraceCheckbox False False 10
  boxPackStart searchHbox searchButton False False 0

  on entry #activate $ buttonClicked searchButton
  on maxSpin #activate $ buttonClicked searchButton
  on tracePercentSpin #activate $ buttonClicked searchButton

  area <- drawingAreaNew
  onWidgetConfigureEvent area $ \ev -> do
      invalidateChart
      return False

  onWidgetButtonPressEvent area $ \ev -> do
      btn <- getEventButtonButton ev
      if btn == 1
        then do
          x <- getEventButtonX ev
          writeIORef selectionRef Nothing
          writeIORef fromXRef $ Just x
          return False
        else return True

  theme <- getTheme window

  let translateTime x = do
        datas <- readIORef dataRef
        showLegend <- askConfig cfgShowLegend cfgRef
        let chart = ChartData title Nothing showLegend (Just theme) datas
        (_, pickFn) <- getChartSurface chartSurfaceRef chart area
        height <- widgetGetAllocatedHeight area
        let y = fromIntegral $ height `div` 2
        case pickFn (Chart.Point x y) of
          Just (LayoutPick_PlotArea time _ _) -> return $ Just time
          _ -> return Nothing

  let detectTimeInterval fromX toX = do
        fromTime <- translateTime fromX
        toTime <- translateTime toX
        case (fromTime, toTime) of
          (Just t1, Just t2) -> return $ Just (t1, t2)
          _ -> return Nothing

  onWidgetButtonReleaseEvent area $ \ev -> do
      btn <- getEventButtonButton ev
      if btn == 1
        then do
          x <- getEventButtonX ev
          mbFromX <- readIORef fromXRef
          whenJust mbFromX $ \fromX -> do
              mbTime <- detectTimeInterval fromX x
              whenJust mbTime $ \(timeFrom, timeTo) ->
                  writeIORef selectionRef $ Just (fromX, x, timeFrom, timeTo)
          writeIORef fromXRef Nothing
          return False
        else return True
      
  onWidgetDraw area $ \ctx -> do
      renderWithContext ctx $ do
          width <- liftIO $ widgetGetAllocatedWidth area
          height <- liftIO $ widgetGetAllocatedHeight area
          style <- liftIO $ widgetGetStyleContext area
          liftIO $ renderBackground style ctx 0 0 (fromIntegral width) (fromIntegral height)

          datas <- liftIO $ readIORef dataRef
          mbHighlight <- liftIO $ readIORef highlightRef

          -- Get previously prepared (off-screen) Surface with already drawn chart or draw a new one
          -- (or draw a new one if there is no one prepared)
          showLegend <- liftIO $ askConfig cfgShowLegend cfgRef
          let chart = ChartData title mbHighlight showLegend (Just theme) datas
          (chartSurface, _) <- liftIO $ getChartSurface chartSurfaceRef chart area
          -- Paint that surface onto the widget
          setSourceSurface chartSurface 0 0
          paint

          mbPointer <- liftIO $ readIORef pointerRef
          whenJust mbPointer $ \ptr -> do
              mbXFrom <- liftIO $ readIORef fromXRef
              whenJust mbXFrom $ \fromX -> do
                  let toX = fst ptr
                  drawSelection area fromX toX

              mbSelection <- liftIO $ readIORef selectionRef
              whenJust mbSelection $ \(fromX, toX, _, _) -> 
                  drawSelection area fromX toX

              drawCross area ptr
      return True

  widgetAddEvents area [GI.Gdk.EventMaskAllEventsMask]

  vbox <- boxNew OrientationVertical 0
  status <- labelNew (Just "Ready.")
  labelSetXalign status 0
  statusBox <- boxNew OrientationHorizontal 0
  setWidgetMargin status 5
  -- setContainerBorderWidth statusBox 10

  zoomInBtn <- buttonNewFromIconName (Just "zoom-in") $ fromIntegral (fromEnum IconSizeMenu)
  widgetSetTooltipText zoomInBtn (Just "Zoom to selection")
  onButtonClicked zoomInBtn $ do
      mbSelection <- readIORef selectionRef
      whenJust mbSelection $ \(_, _, t1, t2) -> do
        prevFilter <- readIORef timeFilterRef
        let fromTime = min t1 t2
            toTime = max t1 t2
        writeIORef timeFilterRef $ (fromTime, toTime) : prevFilter
        writeIORef selectionRef Nothing
        buttonClicked searchButton

  zoomOutBtn <- buttonNewFromIconName (Just "zoom-out") $ fromIntegral (fromEnum IconSizeMenu)
  widgetSetTooltipText zoomOutBtn (Just "Return to previous zoom level")
  onButtonClicked zoomOutBtn $ do
      mbSelection <- readIORef selectionRef
      prevFilter <- readIORef timeFilterRef
      case prevFilter of
        [] -> return ()
        (last : old) -> writeIORef timeFilterRef old
      writeIORef selectionRef Nothing
      buttonClicked searchButton

  zoomResetBtn <- buttonNewFromIconName (Just "zoom-original") $ fromIntegral (fromEnum IconSizeMenu)
  widgetSetTooltipText zoomResetBtn (Just "Reset zoom")
  onButtonClicked zoomResetBtn $ do
      writeIORef timeFilterRef []
      buttonClicked searchButton

  settingsBtn <- buttonNewFromIconName (Just "preferences-system") $ fromIntegral (fromEnum IconSizeMenu)
  widgetSetTooltipText settingsBtn (Just "Preferences")
  onButtonClicked settingsBtn $ do
      ok <- showSettingsDlg window cfgRef
      when ok $
        invalidateChart

  boxPackStart statusBox status True True 0
  boxPackStart statusBox zoomInBtn False False 0
  boxPackStart statusBox zoomOutBtn False False 0
  boxPackStart statusBox zoomResetBtn False False 0
  boxPackStart statusBox settingsBtn False False 0

  boxPackStart vbox searchHbox False False 0
  boxPackStart vbox area True True 0
  boxPackStart vbox statusBox False False 0

  onWidgetMotionNotifyEvent area $ \ev -> do
      x <- getEventMotionX ev
      y <- getEventMotionY ev
      datas <- readIORef dataRef
      showLegend <- askConfig cfgShowLegend cfgRef
      let chart = ChartData title Nothing showLegend (Just theme) datas
      (_, pickFn) <- getChartSurface chartSurfaceRef chart area
      case pickFn (Chart.Point x y) of
        Just (LayoutPick_PlotArea x y _) -> do
          case searchKey datas x y of
            Just (key, x, dy) -> do
                let bytes = hValueUnit (heapHeader heap)
                    seconds = hSampleUnit (heapHeader heap)
                let text = format "{}: {} {} at {:.2} {}" (key, formatNum bytesFormat dy, bytes, x, seconds)
                labelSetText status (TL.toStrict text)
                mbPrevKey <- readIORef highlightRef
                doHighlight <- askConfig cfgHighlight cfgRef
                when doHighlight $
                  when (mbPrevKey /= Just key) $ do
                    writeIORef highlightRef (Just key)
            _ -> labelSetText status ""
        _ -> labelSetText status ""
      writeIORef pointerRef $ Just (x,y)
      widgetQueueDraw area
      return True

  on searchButton #clicked $ do
      text <- entryGetText entry
      Just fieldId <- comboBoxGetActiveId searchFieldCombo
      let field = read $ T.unpack fieldId
      Just methodId <- comboBoxGetActiveId searchMethodCombo
      let method = read $ T.unpack methodId
      maxN <- spinButtonGetValueAsInt maxSpin
      drawTrace <- toggleButtonGetActive drawTraceCheckbox
      tracePercent <- spinButtonGetValueAsInt tracePercentSpin
      Just traceStyleId <- comboBoxGetActiveId traceStyleCombo
      let traceStyle = read $ T.unpack traceStyleId
      timeFilters <- readIORef timeFilterRef
      let mbTimeFilter = case timeFilters of
                           [] -> Nothing
                           (fltr : _) -> Just fltr
      nSamples <- askConfig cfgSamplesNr cfgRef
      let datas = allSamplesData $ resampleHeap nSamples $ filterHeap mbTimeFilter (fromIntegral maxN) traceStyle (fromIntegral tracePercent) (checkItem field method text) drawTrace heap
      writeIORef dataRef datas
      invalidateChart
      widgetQueueDraw area

  setContainerChild window vbox
  widgetShowAll window
  -- All Gtk+ applications must run the main event loop. Control ends here and
  -- waits for an event to occur (like a key press or mouse event).
  GI.main

convertColor :: GI.Gdk.RGBA -> IO (AlphaColour Double)
convertColor rgba = do
    r <- getRGBARed rgba
    g <- getRGBAGreen rgba
    b <- getRGBABlue rgba
    return $ opaque $ sRGB r g b

getTheme :: Window -> IO ChartTheme
getTheme w = do
  style <- widgetGetStyleContext w
  foreground <- convertColor =<< styleContextGetColor style [StateFlagsNormal]
  return $ ChartTheme foreground

drawCross :: DrawingArea -> (Double, Double) -> Render ()
drawCross area (xc, yc) = do
  width <- liftIO $ widgetGetAllocatedWidth area
  height <- liftIO $ widgetGetAllocatedHeight area

  setDash [8, 2] 0
  setSourceRGBA 0 0 0 0.5
  Cairo.moveTo 0 yc
  Cairo.lineTo (fromIntegral width) yc
  stroke
  Cairo.moveTo xc 0
  Cairo.lineTo xc (fromIntegral height)
  stroke
  setDash [] 0

drawSelection :: DrawingArea -> Double -> Double -> Render ()
drawSelection area fromX toX = do
  width <- liftIO $ widgetGetAllocatedWidth area
  height <- liftIO $ widgetGetAllocatedHeight area

  rectangle fromX 0 (toX - fromX) (fromIntegral height)
  setSourceRGBA 0 0 0.5 0.2
  fill

getChartSurface :: IORef ChartCache -> ChartData -> DrawingArea -> IO (Surface, LayoutPickFn)
getChartSurface surfaceRef chart area = do
    cache <- readIORef surfaceRef
    case M.lookup (chtHighlgiht chart) cache of
      Nothing -> do
        -- this should only be executed in case when onWidgetDraw is called first time
        -- for this mbHighlight after cache is invalidated
        (surface, fn) <- drawChartOffscreen surfaceRef chart area
        writeIORef surfaceRef $ M.insert (chtHighlgiht chart) (surface, fn) cache
        return (surface, fn)

      Just (surface, fn) -> return (surface, fn)

drawChartOffscreen :: IORef ChartCache -> ChartData -> DrawingArea -> IO (Surface, LayoutPickFn)
drawChartOffscreen surfaceRef chart area = do
      surface <- recreateSurface 
      fn <- renderWith surface $ drawChart chart area
      surfaceFlush surface
      cache <- readIORef surfaceRef
      writeIORef surfaceRef $ M.insert (chtHighlgiht chart) (surface, fn) cache
      -- putStrLn $ "chart drawn: " ++ show mbHighlight
      return (surface, fn)
  where
    recreateSurface = do
      cache <- readIORef surfaceRef
      case M.lookup (chtHighlgiht chart) cache of
        Just (oldSurface, _) -> return oldSurface
        Nothing -> createSurface

    createSurface = do
      width <- widgetGetAllocatedWidth area
      height <- widgetGetAllocatedHeight area
      createImageSurface FormatARGB32 (fromIntegral width) (fromIntegral height)
      
drawChart :: ChartData -> DrawingArea -> Render (PickFn (LayoutPick Double Int Int))
drawChart chart area = do
  width <- liftIO $ widgetGetAllocatedWidth area
  height <- liftIO $ widgetGetAllocatedHeight area
  renderChart (makeChart chart) (width, height)

renderChart :: Chart.Layout Double Int -> (Int32, Int32) -> Render (PickFn (LayoutPick Double Int Int))
renderChart chart (width, height) = do
  let sz = (fromIntegral width, fromIntegral height)
  runBackend (defaultEnv bitmapAlignmentFns) (render (layoutToRenderable chart) sz)

mkComboBox :: (Show a) => [(a, T.Text)] -> IO ComboBoxText
mkComboBox pairs = do
  combo <- comboBoxTextNew
  forM_ pairs $ \(value, title) -> do
    let id = T.pack (show value)
    comboBoxTextAppend combo (Just id) title
  comboBoxSetActive combo 0
  return combo

