{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad
import Control.Monad.State
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import Data.Attoparsec.Text.Lazy

import Types

data ParserState = ParserState {
    psHeader :: Header
  , psHeaderParsed :: Bool
  , psSamples :: [Sample ItemsMap]
  , psCurrentSample :: Sample ItemsMap
  }

data LineData =
    JOB TL.Text
  | DATE TL.Text
  | SAMPLE_UNIT TL.Text
  | VALUE_UNIT TL.Text
  | BEGIN_SAMPLE Double
  | END_SAMPLE Double
  | DATA TL.Text Int
  deriving (Show)

initState :: ParserState
initState = ParserState zeroHeader False [] undefined

zeroHeader :: Header
zeroHeader = Header "" "" "" ""

parseHeap :: TL.Text -> Heap
parseHeap text =
    let st = execState runParser initState
        runParser = forM_ (TL.lines text) parseLine
    in  Heap (psHeader st) (reverse $ psSamples st)

processLine :: LineData -> State ParserState ()
processLine (JOB job) = modify $ \st -> st {psHeader = (psHeader st) {hJob = TL.toStrict job}}
processLine (DATE date) = modify $ \st -> st {psHeader = (psHeader st) {hDate = TL.toStrict date}}
processLine (SAMPLE_UNIT unit) = modify $ \st -> st {psHeader = (psHeader st) {hSampleUnit = TL.toStrict unit}}
processLine (VALUE_UNIT unit) =
  modify $ \st -> st {
        psHeader = (psHeader st) {hValueUnit = TL.toStrict unit}
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
          in  (psCurrentSample st) {sampleItems = M.insert (TL.toStrict name) value items}
    }

begin_sample :: TL.Text
begin_sample = "BEGIN_SAMPLE"

end_sample :: TL.Text
end_sample = "END_SAMPLE"

parseLine :: TL.Text -> State ParserState ()
parseLine line = do
    st <- get
    if not (psHeaderParsed st)
      then do
        case eitherResult $ parse pHeaderLine line of
          Left err -> fail $ "Can't parse header line: " ++ show line ++ ": " ++ err
          Right lineData -> processLine lineData
      else 
        if begin_sample `TL.isPrefixOf` line
          then processLine =<< BEGIN_SAMPLE <$> parseSample begin_sample line
          else if end_sample `TL.isPrefixOf` line
                 then processLine =<< END_SAMPLE <$> parseSample end_sample line
                 else do
                  (name, value) <- parseItem line
                  processLine $ DATA name value

-- | Parse BEGIN_SAMPLE or END_SAMPLE line
parseSample :: Monad m => TL.Text -> TL.Text -> m Double
parseSample name line = do
  let rest = TL.drop (TL.length name + 1) line
  case eitherResult $ parse pDouble rest of
    Left err -> fail $ "Can't parse double: " ++ show rest ++ ": " ++ err
    Right value -> return value

-- | Parse data item line
parseItem :: Monad m => TL.Text -> m (TL.Text, Int)
parseItem line = do
  let (name, rest) = TL.break (== '\t') line
  if TL.null rest
    then fail $ "Can't parse data line: " ++ show line
    else case eitherResult $ parse decimal (TL.tail rest) of
           Left err -> fail $ "can't parse decimal: " ++ show rest ++ ": " ++ err
           Right value -> return (name, value)
    
pHeaderLine :: Parser LineData
pHeaderLine =
      (JOB <$> pStringValue "JOB")
  <|> (DATE <$> pStringValue "DATE")
  <|> (SAMPLE_UNIT <$> pStringValue "SAMPLE_UNIT")
  <|> (VALUE_UNIT <$> pStringValue "VALUE_UNIT")

pString :: Parser TL.Text
pString = do  
  char '"'
  string <- manyTill anyChar $ char '"'
  return $ TL.pack string

pStringValue :: T.Text -> Parser TL.Text
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

