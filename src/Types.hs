
module Types where

import qualified Data.Text as T
import qualified Data.Map as M

data Heap = Heap {
    heapHeader :: ! Header
  , heapSamples :: ! [Sample ItemsMap]
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

