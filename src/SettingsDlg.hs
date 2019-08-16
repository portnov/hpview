{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module SettingsDlg where

import Control.Monad
import Data.Default.Class
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

import Types
import Config

showSettingsDlg :: Window -> IORef Config -> IO Bool
showSettingsDlg win cfgRef = do
    dlg <- dialogNew
    windowSetTitle dlg "Preferences"
    windowSetModal dlg True
    windowSetTransientFor dlg (Just win)
    dialogAddButton dlg "Ok" (fromIntegral $ fromEnum ResponseTypeOk)
    area <- dialogGetContentArea dlg

    showLegendCheckbox <- checkButtonNewWithLabel "Show legend"
    highlightCheckbox <- checkButtonNewWithLabel "Highlight area under pointer"

    cfg <- readIORef cfgRef
    toggleButtonSetActive showLegendCheckbox (cfgShowLegend cfg)
    toggleButtonSetActive highlightCheckbox (cfgHighlight cfg)

    setContainerBorderWidth area 10
    boxPackStart area showLegendCheckbox False False 0
    boxPackStart area highlightCheckbox False False 0
    widgetShowAll dlg

    response <- dialogRun dlg
    widgetDestroy dlg
    if toEnum (fromIntegral response) == ResponseTypeOk
      then do
        putStrLn "okay"
        showLegend <- toggleButtonGetActive showLegendCheckbox
        highlight <- toggleButtonGetActive highlightCheckbox
        let cfg' = Config showLegend highlight
        writeIORef cfgRef cfg'
        saveConfig cfg'
        return True
      else return False

