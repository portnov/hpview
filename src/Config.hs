{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.Text as T
import Data.IORef
import Data.Yaml
import Data.Aeson.Types
import Data.Default.Class
import System.Environment
import System.FilePath
import System.Directory

import Types

instance Default Config where
  def = Config True True (Just 500)

instance ToJSON Config where
  toJSON cfg = object [
                  "show-legend" .= cfgShowLegend cfg
                , "highlight-selected" .= cfgHighlight cfg
                , "samples-count" .= cfgSamplesNr cfg
                ]

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
      <$> v .: "show-legend"
      <*> v .: "highlight-selected"
      <*> v .: "samples-count"

getConfigPath :: IO FilePath
getConfigPath = do
  home <- getEnv "HOME"
  let path = home </> ".config" </> "hpview.yaml"
  return path

loadConfig :: IO Config
loadConfig = do
  path <- getConfigPath
  ex <- doesFileExist path
  if ex
    then do
      r <- decodeFileEither path
      case r of
        Left err -> do
          putStrLn $ "Can't parse config file: " ++ show err
          putStrLn "will use default config."
          return def
        Right cfg -> return cfg
    else return def

saveConfig :: Config -> IO ()
saveConfig cfg = do
  path <- getConfigPath
  encodeFile path cfg

askConfig :: (Config -> a) -> IORef Config -> IO a
askConfig fn cfgRef = do
  cfg <- readIORef cfgRef
  return $ fn cfg

