{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module SettingsDlg where

import Control.Monad
import Data.Default.Class
import Data.Int
import Data.IORef
import Data.Maybe
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
    useSamplesCheckbox <- checkButtonNewWithLabel "Use downsampling"

    samplesBox <- boxNew OrientationHorizontal 10
    samplesLbl <- labelNew (Just "Samples count:")
    samplesSpin <- spinButtonNewWithRange 100 10000 1
    boxPackStart samplesBox samplesLbl True True 10
    boxPackStart samplesBox samplesSpin False False 0

    cfg <- readIORef cfgRef
    toggleButtonSetActive showLegendCheckbox (cfgShowLegend cfg)
    toggleButtonSetActive highlightCheckbox (cfgHighlight cfg)
    toggleButtonSetActive useSamplesCheckbox (isJust $ cfgSamplesNr cfg)
    spinButtonSetValue samplesSpin $ fromIntegral $ fromMaybe 500 (cfgSamplesNr cfg)

    setContainerBorderWidth area 10
    boxPackStart area showLegendCheckbox False False 0
    boxPackStart area highlightCheckbox False False 0
    boxPackStart area useSamplesCheckbox False False 0
    boxPackStart area samplesBox False False 0
    widgetShowAll dlg

    response <- dialogRun dlg
    widgetDestroy dlg
    if toEnum (fromIntegral response) == ResponseTypeOk
      then do
        putStrLn "okay"
        showLegend <- toggleButtonGetActive showLegendCheckbox
        highlight <- toggleButtonGetActive highlightCheckbox
        useSamples <- toggleButtonGetActive useSamplesCheckbox
        samplesNr <- spinButtonGetValueAsInt samplesSpin
        let samples = if useSamples
                        then Just $ fromIntegral samplesNr
                        else Nothing
        let cfg' = Config showLegend highlight samples
        writeIORef cfgRef cfg'
        saveConfig cfg'
        return True
      else return False

