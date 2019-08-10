{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Attoparsec.Text

import Types

pString :: Parser T.Text
pString = do  
  char '"'
  string <- manyTill anyChar $ char '"'
  return $ T.pack string

pStringValue :: T.Text -> Parser T.Text
pStringValue name = do
  string name
  space
  value <- pString
  endOfLine
  return value

pHeader :: Parser Header
pHeader = Header
  <$> pStringValue "JOB"
  <*> pStringValue "DATE"
  <*> pStringValue "SAMPLE_UNIT"
  <*> pStringValue "VALUE_UNIT"

pItem :: Parser (T.Text, Int)
pItem = do
  name <- manyTill anyChar $ char '\t'
  let nameT = T.pack name
  guard $ not $ "END_SAMPLE" `T.isPrefixOf` nameT
  value <- decimal
  endOfLine
  return (nameT, value)

pSample :: Parser (Sample ItemsMap)
pSample = do
  string "BEGIN_SAMPLE"
  space
  time <- double
  endOfLine
  items <- many pItem
  string "END_SAMPLE"
  space
  time1 <- double
  guard $ time1 == time
  endOfLine
  return $ Sample time (M.fromList items)

pHeap :: Parser Heap
pHeap = Heap
  <$> pHeader
  <*> many pSample

