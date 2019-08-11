
module Gui where

import Control.Monad
import Control.Lens
import Data.Default.Class
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Int
import Data.IORef
import qualified GI.Gtk as GI (main, init)
import qualified GI.Gdk
import GI.Gdk.Structs
import GI.Gtk hiding (main)
import qualified GI.Cairo
import Graphics.Rendering.Chart as Chart
import Graphics.Rendering.Cairo
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
  let datas = allSamplesData heap

  area <- drawingAreaNew
  onWidgetDraw area $ \ctx -> do
      renderWithContext ctx $ do
          fn <- drawChart area datas
          liftIO $ writeIORef pickFnRef fn
      return True

  widgetAddEvents area [GI.Gdk.EventMaskAllEventsMask]

  onWidgetButtonPressEvent area $ \ev -> do
      x <- getEventButtonX ev
      y <- getEventButtonY ev
      pickFn <- readIORef pickFnRef
      case pickFn (Chart.Point x y) of
        Just (LayoutPick_PlotArea x y _) -> do
          print (x,y)
          case searchKey datas x y of
            Just key -> print key
            _ -> return ()
        _ -> return ()
      return True

  setContainerChild window area
  widgetShowAll window
  -- All Gtk+ applications must run the main event loop. Control ends here and
  -- waits for an event to occur (like a key press or mouse event).
  GI.main

drawChart :: DrawingArea -> SamplesData -> Render (PickFn (LayoutPick Double Int Int))
drawChart area datas = do
  width <- liftIO $ widgetGetAllocatedWidth area
  height <- liftIO $ widgetGetAllocatedHeight area
  renderChart (makeChart datas) (width, height)

renderChart :: Chart.Layout Double Int -> (Int32, Int32) -> Render (PickFn (LayoutPick Double Int Int))
renderChart chart (width, height) = do
  let sz = (fromIntegral width, fromIntegral height)
  runBackend (defaultEnv bitmapAlignmentFns) (render (layoutToRenderable chart) sz)

