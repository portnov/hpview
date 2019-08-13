{-# LANGUAGE OverloadedStrings #-}

module Gui where

import Control.Monad
import Control.Lens
import Data.Default.Class
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Int
import Data.IORef
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

import Types
import Chart
import Operations
import GiCairoBridge

runWindow :: Heap -> IO ()
runWindow heap = do
  GI.init Nothing
  -- Create a new window
  window <- windowNew WindowTypeToplevel
  -- Here we connect the "destroy" event to a signal handler.
  onWidgetDestroy window mainQuit

  pickFnRef <- newIORef (const Nothing)
  highlightRef <- newIORef Nothing
  pointerRef <- newIORef Nothing
  let datas = allSamplesData $ filterHeap 10 (const True) heap
  let title = hJob (heapHeader heap) <> " at " <> hDate (heapHeader heap)

  area <- drawingAreaNew
  onWidgetDraw area $ \ctx -> do
      renderWithContext ctx $ do
          mbHighlight <- liftIO $ readIORef highlightRef
          fn <- drawChart title mbHighlight area datas
          liftIO $ writeIORef pickFnRef fn
          mbPointer <- liftIO $ readIORef pointerRef
          case mbPointer of
            Nothing -> return ()
            Just ptr -> drawCross area ptr
      return True

  widgetAddEvents area [GI.Gdk.EventMaskAllEventsMask]

  vbox <- boxNew OrientationVertical 0
  boxPackStart vbox area True True 0

  status <- statusbarNew
  statusContext <- statusbarGetContextId status "Status"
  boxPackStart vbox status False False 0

  onWidgetMotionNotifyEvent area $ \ev -> do
      x <- getEventMotionX ev
      y <- getEventMotionY ev
      pickFn <- readIORef pickFnRef
      case pickFn (Chart.Point x y) of
        Just (LayoutPick_PlotArea x y _) -> do
          case searchKey datas x y of
            Just (key, x, dy) -> do
                let bytes = hValueUnit (heapHeader heap)
                    seconds = hSampleUnit (heapHeader heap)
                let text = format "{}: {} {} at {:.2} {}" (key, dy, bytes, x, seconds)
                statusbarPush status statusContext (TL.toStrict text)
                mbPrevKey <- readIORef highlightRef
                writeIORef highlightRef (Just key)
            _ -> statusbarRemoveAll status statusContext
        _ -> statusbarRemoveAll status statusContext
      writeIORef pointerRef $ Just (x,y)
      widgetQueueDraw area
      return True

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

drawChart :: T.Text -> Maybe T.Text -> DrawingArea -> SamplesData -> Render (PickFn (LayoutPick Double Int Int))
drawChart title mbHighlight area datas = do
  width <- liftIO $ widgetGetAllocatedWidth area
  height <- liftIO $ widgetGetAllocatedHeight area
  renderChart (makeChart title mbHighlight datas) (width, height)

renderChart :: Chart.Layout Double Int -> (Int32, Int32) -> Render (PickFn (LayoutPick Double Int Int))
renderChart chart (width, height) = do
  let sz = (fromIntegral width, fromIntegral height)
  runBackend (defaultEnv bitmapAlignmentFns) (render (layoutToRenderable chart) sz)

