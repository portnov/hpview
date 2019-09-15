{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad
import Control.Monad.State
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as TR
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Attoparsec.Text

import Types
import Operations (calcNameWeight, allKeys)
import Algebra

data ParserState = ParserState {
    psHeader :: ! Header
  , psHeaderParsed :: ! Bool
  , psSamples :: ! [Sample ItemsMap]
  , psCurrentSample :: ! (Sample ItemsMap)
  }

data LineData =
    JOB ! T.Text
  | DATE ! T.Text
  | SAMPLE_UNIT ! T.Text
  | VALUE_UNIT ! T.Text
  | BEGIN_SAMPLE ! Double
  | END_SAMPLE ! Double
  | DATA ! T.Text ! Int
  deriving (Show)

initState :: ParserState
initState = ParserState zeroHeader False [] (Sample 0 M.empty)

zeroHeader :: Header
zeroHeader = Header "" "" "" ""

parseHeap :: T.Text -> Heap
parseHeap text =
    let st = execState runParser initState
        runParser = forM_ (T.lines text) parseLine
        samples = reverse $ psSamples st
        weights = M.unionsWith (+) $ map sampleItems $ psSamples st
        coeffs = M.fromList [(key, growCoefficient  key (heapSamples heap)) | key <- allKeys heap]
        heap = Heap (psHeader st) samples weights coeffs
    in  heap

processLine :: LineData -> State ParserState ()
processLine (JOB job) = modify $ \st -> st {psHeader = (psHeader st) {hJob = job}}
processLine (DATE date) = modify $ \st -> st {psHeader = (psHeader st) {hDate = date}}
processLine (SAMPLE_UNIT unit) = modify $ \st -> st {psHeader = (psHeader st) {hSampleUnit = unit}}
processLine (VALUE_UNIT unit) =
  modify $ \st -> st {
        psHeader = (psHeader st) {hValueUnit = unit}
      , psHeaderParsed = True
    }
processLine (BEGIN_SAMPLE time) =
  modify $ \st -> st {psCurrentSample = Sample time M.empty}
processLine (END_SAMPLE _) =
  modify $ \st -> st {psSamples = psCurrentSample st : psSamples st}
processLine (DATA name value) =
  modify $ \st -> st {
      psCurrentSample =
          let items = sampleItems (psCurrentSample st)
          in  (psCurrentSample st) {sampleItems = M.insert name value items}
    }

begin_sample :: T.Text
begin_sample = "BEGIN_SAMPLE"

end_sample :: T.Text
end_sample = "END_SAMPLE"

parseLine :: T.Text -> State ParserState ()
parseLine line = do
    st <- get
    if not (psHeaderParsed st)
      then do
        case eitherResult $ parse pHeaderLine line of
          Left err -> fail $ "Can't parse header line: " ++ show line ++ ": " ++ err
          Right lineData -> processLine lineData
      else 
        if begin_sample `T.isPrefixOf` line
          then processLine =<< BEGIN_SAMPLE <$> parseSample begin_sample line
          else if end_sample `T.isPrefixOf` line
                 then processLine =<< END_SAMPLE <$> parseSample end_sample line
                 else do
                  (name, value) <- parseItem line
                  processLine $ DATA name value

-- | Parse BEGIN_SAMPLE or END_SAMPLE line
parseSample :: Monad m => T.Text -> T.Text -> m Double
parseSample name line = do
  let rest = T.drop (T.length name + 1) line
  case parseOnly (pDouble <* endOfInput) rest of
    Left err -> fail $ "Can't parse double: " ++ show rest ++ ": " ++ err
    Right value -> return value

-- | Parse data item line
parseItem :: Monad m => T.Text -> m (T.Text, Int)
parseItem line = do
  let (name, rest) = T.break (== '\t') line
  if T.null rest
    then fail $ "Can't parse data line: " ++ show line
    else case TR.decimal (T.tail rest) of
           Left err -> fail $ "can't parse decimal: " ++ show rest ++ ": " ++ err
           Right (value, "") -> return (name, value)
    
pHeaderLine :: Parser LineData
pHeaderLine =
      (JOB <$> pStringValue "JOB")
  <|> (DATE <$> pStringValue "DATE")
  <|> (SAMPLE_UNIT <$> pStringValue "SAMPLE_UNIT")
  <|> (VALUE_UNIT <$> pStringValue "VALUE_UNIT")

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
  return value

-- for some reason, Haskell programs sometimes use
-- comma as decimal separator instead of dot.
-- TODO: this is not a very effective parser :/
pDouble :: Parser Double
pDouble = doubleComma <|> double
  where
    doubleComma = do
      int <- decimal
      char ','
      zeros <- many $ char '0'
      mantis <- decimal
      let n = length zeros + length (show mantis)
      return $ fromIntegral int + fromIntegral mantis / 10^n

