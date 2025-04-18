{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module TogglRequest
  ( startTimeEntry
  , getCurrentTimeEntryId
  , stopTimeEntry
  ) where

import Network.HTTP.Req
import Data.Text (Text, unpack, pack)
import Data.Aeson (ToJSON, encode, Value, (.:), withObject)
import Data.Aeson.Types (parseMaybe)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as BS
import GHC.Generics (Generic)

data TogglRequest = TogglRequest
  { description :: Text
  , project_id :: Integer
  , created_with :: Text
  , duration :: Integer
  , workspace_id :: Integer
  , start :: Text
  , stop :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON TogglRequest

startTimeEntry :: Text -> Text -> Text -> Text -> IO ()
startTimeEntry apiKey workspaceId projectId desc = runReq defaultHttpConfig $ do
    currentTime <- liftIO getCurrentTime
    let isoTime = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" currentTime
        togglData = TogglRequest
          { description = desc
            , project_id = read (unpack projectId) :: Integer
            , created_with = "loggman"
            , duration = -1
            , workspace_id = read (unpack workspaceId) :: Integer
            , start = isoTime
            , stop = Nothing
          }

    let endpoint = https "api.track.toggl.com" /: "api" /: "v9" /: "workspaces" /: workspaceId /: "time_entries"
        authHeader = basicAuth (BS.pack $ unpack apiKey) "api_token"
    response <- req POST endpoint (ReqBodyLbs $ encode togglData) jsonResponse authHeader :: Req (JsonResponse Value)
    liftIO $ BL.putStrLn (encode $ responseBody response)

getCurrentTimeEntryId :: Text -> IO (Maybe Integer)
getCurrentTimeEntryId apiKey = runReq defaultHttpConfig $ do
    let endpoint = https "api.track.toggl.com" /: "api" /: "v9" /: "me" /: "time_entries" /: "current"
        authHeader = basicAuth (BS.pack $ unpack apiKey) "api_token"
    response <- req GET endpoint NoReqBody jsonResponse authHeader :: Req (JsonResponse Value)
    let responseBodyValue = responseBody response
    return $ parseMaybe (withObject "current time entry" (\obj -> obj .: "id")) responseBodyValue

stopTimeEntry :: Text -> Text -> Integer -> IO ()
stopTimeEntry apiKey workspaceId timeEntryId = runReq defaultHttpConfig $ do
    let endpoint = https "api.track.toggl.com" /: "api" /: "v9" /: "workspaces" /: workspaceId /: "time_entries" /: pack (show timeEntryId) /: "stop"
        authHeader = basicAuth (BS.pack $ unpack apiKey) "api_token"
    response <- req PATCH endpoint NoReqBody jsonResponse authHeader :: Req (JsonResponse Value)
    liftIO $ BL.putStrLn (encode $ responseBody response)
