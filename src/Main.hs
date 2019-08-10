{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Applicative
import qualified Data.Text.IO as TIO
import Data.Attoparsec.Text
import System.Environment

import Types
import Parser
import Gui
import Operations

main :: IO ()
main = do
  [file] <- getArgs
  text <- TIO.readFile file
--   case parseOnly pSample text of
--     Left err -> fail err
--     Right sample -> print sample
--   case parseOnly (many pSample) text of
--     Left err -> fail err
--     Right sample -> print sample
  case parseOnly pHeap text of
    Left err -> fail err
    Right heap -> do
      print $ heapHeader heap
      print $ length $ heapSamples heap

      runWindow heap

