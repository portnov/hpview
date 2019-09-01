{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Default.Class
import Data.Colour
import Formattable.NumFormat

data Heap = Heap {
    heapHeader :: ! Header
  , heapSamples :: ! [Sample ItemsMap]
  , heapWeights :: M.Map T.Text Int
  }
  deriving (Eq, Show)

data Header = Header {
    hJob :: ! T.Text
  , hDate :: ! T.Text
  , hSampleUnit :: ! T.Text
  , hValueUnit :: ! T.Text
  }
  deriving (Eq, Show)

data Item = Item {
    itemName :: ! T.Text
  , itemValue :: ! Int
  }
  deriving (Eq, Show)

type ItemsMap = (M.Map T.Text Int)

data Sample m = Sample {
    sampleTime :: ! Double
  , sampleItems :: ! m
  }
  deriving (Eq, Show)

data NameInfo = NameInfo {
    niPackage :: T.Text
  , niFullName :: T.Text
  , niModule :: T.Text
  , niName :: T.Text
  }
  deriving (Eq, Show)

data SearchMethod = Contains | Exact | Regexp
  deriving (Eq, Show, Read, Enum, Bounded)

data SearchField = Name | Module | Package
  deriving (Eq, Show, Read, Enum, Bounded)

data TraceStyle = TraceTotal | TraceEach
  deriving (Eq, Show, Read, Enum, Bounded)

bytesFormat :: NumFormat
bytesFormat = def {
    _nfThouSep = " ",
    _nfPrec = Just (0, Decimals),
    _nfStyle = SIStyle,
    _nfSuffix = "B"
  }

data Config = Config {
      cfgShowLegend :: Bool
    , cfgHighlight :: Bool
    , cfgSamplesNr :: Maybe Int
  }
  deriving (Eq, Show)

type SamplesData = [(T.Text, [(Double, (Int, Int))])]

data ChartTheme = ChartTheme {
    thmForeground :: AlphaColour Double
  }

data ChartData = ChartData {
      chtTitle :: T.Text
    , chtHighlgiht :: Maybe T.Text
    , chtLegend :: Bool
    , chtTheme :: Maybe ChartTheme
    , chtSamples :: SamplesData
  }

