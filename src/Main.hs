{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Applicative
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import Data.Attoparsec.Text
import System.Environment

import Types
import Parser
import Gui
import Operations

main :: IO ()
main = do
  [file] <- getArgs
  text <- TLIO.readFile file
  let heap = parseHeap text
  print $ heapHeader heap
  print $ length $ heapSamples heap

  -- runWindow heap

