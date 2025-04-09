{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ConfigLoader
  ( Config(..)
  , TogglConfig(..)
  , loadConfig
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Yaml (decodeFileEither, ParseException, FromJSON)
import Data.HashMap.Strict (HashMap)

data TogglConfig = TogglConfig
  { apiKey :: Text
  , projectIds :: HashMap Text Text
  } deriving (Show, Generic)

data Config = Config
  { toggl :: TogglConfig
  } deriving (Show, Generic)

instance FromJSON TogglConfig
instance FromJSON Config

loadConfig :: FilePath -> IO (Either ParseException Config)
loadConfig configPath = decodeFileEither configPath
