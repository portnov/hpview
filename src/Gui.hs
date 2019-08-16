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
import Operations
import GiCairoBridge

dfltTracePercent :: Int
dfltTracePercent = 1

type LayoutPickFn = PickFn (LayoutPick Double Int Int)
type ChartCache = M.Map (Maybe T.Text) (Surface, LayoutPickFn)

runWindow :: Heap -> IO ()
runWindow heap = do
  GI.init Nothing
  -- Create a new window
  window <- windowNew WindowTypeToplevel
  -- Here we connect the "destroy" event to a signal handler.
  onWidgetDestroy window mainQuit

  highlightRef <- newIORef Nothing
  pointerRef <- newIORef Nothing
  chartSurfaceRef <- newIORef M.empty
  let allDatas = allSamplesData $ filterHeap 10 TraceTotal dfltTracePercent (const True) True heap
  dataRef <- newIORef allDatas
  
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
      -- invalidate existing surface, if any
      writeIORef chartSurfaceRef M.empty
      return False
      
  onWidgetDraw area $ \ctx -> do
      renderWithContext ctx $ do
          datas <- liftIO $ readIORef dataRef
          mbHighlight <- liftIO $ readIORef highlightRef

          -- Get previously prepared (off-screen) Surface with already drawn chart or draw a new one
          (chartSurface, _) <- liftIO $ getChartSurface chartSurfaceRef title mbHighlight area datas
          -- Paint that surface onto the widget
          setSourceSurface chartSurface 0 0
          paint

          mbPointer <- liftIO $ readIORef pointerRef
          case mbPointer of
            Nothing -> return ()
            Just ptr -> drawCross area ptr
      return True

  widgetAddEvents area [GI.Gdk.EventMaskAllEventsMask]

  vbox <- boxNew OrientationVertical 0
  status <- statusbarNew
  statusContext <- statusbarGetContextId status "Status"

  boxPackStart vbox searchHbox False False 0
  boxPackStart vbox area True True 0
  boxPackStart vbox status False False 0

  onWidgetMotionNotifyEvent area $ \ev -> do
      x <- getEventMotionX ev
      y <- getEventMotionY ev
      datas <- readIORef dataRef
      (_, pickFn) <- getChartSurface chartSurfaceRef title Nothing area datas
      case pickFn (Chart.Point x y) of
        Just (LayoutPick_PlotArea x y _) -> do
          case searchKey datas x y of
            Just (key, x, dy) -> do
                let bytes = hValueUnit (heapHeader heap)
                    seconds = hSampleUnit (heapHeader heap)
                let text = format "{}: {} {} at {:.2} {}" (key, formatNum bytesFormat dy, bytes, x, seconds)
                statusbarPush status statusContext (TL.toStrict text)
                mbPrevKey <- readIORef highlightRef
                when (mbPrevKey /= Just key) $ do
                  -- drawChartOffscreen chartSurfaceRef title (Just key) area datas
                  writeIORef highlightRef (Just key)
            _ -> statusbarRemoveAll status statusContext
        _ -> statusbarRemoveAll status statusContext
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
    let datas = allSamplesData $ filterHeap (fromIntegral maxN) traceStyle (fromIntegral tracePercent) (checkItem field method text) drawTrace heap
    writeIORef dataRef datas
    -- invalidate existing surface, if any
    writeIORef chartSurfaceRef M.empty
    widgetQueueDraw area

  setContainerChild window vbox
  widgetShowAll window
  -- All Gtk+ applications must run the main event loop. Control ends here and
  -- waits for an event to occur (like a key press or mouse event).
  GI.main

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

getChartSurface :: IORef ChartCache ->  T.Text -> Maybe T.Text -> DrawingArea -> SamplesData -> IO (Surface, LayoutPickFn)
getChartSurface surfaceRef title mbHighlight area datas = do
    cache <- readIORef surfaceRef
    case M.lookup mbHighlight cache of
      Nothing -> do
        -- this should only be executed in case when onWidgetDraw is called first time
        -- for this mbHighlight after cache is invalidated
        (surface, fn) <- drawChartOffscreen surfaceRef title mbHighlight area datas
        writeIORef surfaceRef $ M.insert mbHighlight (surface, fn) cache
        return (surface, fn)

      Just (surface, fn) -> return (surface, fn)

drawChartOffscreen :: IORef ChartCache -> T.Text -> Maybe T.Text -> DrawingArea -> SamplesData -> IO (Surface, LayoutPickFn)
drawChartOffscreen surfaceRef title mbHighlight area datas = do
      surface <- recreateSurface 
      fn <- renderWith surface $ drawChart title mbHighlight area datas
      surfaceFlush surface
      cache <- readIORef surfaceRef
      writeIORef surfaceRef $ M.insert mbHighlight (surface, fn) cache
      -- putStrLn $ "chart drawn: " ++ show mbHighlight
      return (surface, fn)
  where
    recreateSurface = do
      cache <- readIORef surfaceRef
      case M.lookup mbHighlight cache of
        Just (oldSurface, _) -> return oldSurface
        Nothing -> createSurface

    createSurface = do
      width <- widgetGetAllocatedWidth area
      height <- widgetGetAllocatedHeight area
      createImageSurface FormatARGB32 (fromIntegral width) (fromIntegral height)
      
drawChart :: T.Text -> Maybe T.Text -> DrawingArea -> SamplesData -> Render (PickFn (LayoutPick Double Int Int))
drawChart title mbHighlight area datas = do
  width <- liftIO $ widgetGetAllocatedWidth area
  height <- liftIO $ widgetGetAllocatedHeight area
  renderChart (makeChart title mbHighlight datas) (width, height)

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

