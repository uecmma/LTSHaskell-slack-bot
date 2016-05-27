{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Types where

import Data.Aeson

type Channel = String
type UserName = String
type IconEmoji = String
type URL = String
type LTSVersion = String


data SlackMessage = SlackMessage {
  getChannel :: Channel
, getUserName :: UserName
, getText :: String
, getIconEmoji :: IconEmoji
} deriving (Show, Eq)

instance ToJSON SlackMessage where
  toJSON sm = object ["channel" .= getChannel sm,
                      "username" .= getUserName sm,
                      "text" .= getText sm,
                      "icon_emoji" .= getIconEmoji sm]


data LTSInfo = LTSInfo {
  gLTS :: LTSVersion
, gLTS2 :: LTSVersion
, gLTS3 :: LTSVersion
, gLTS4 :: LTSVersion
, gLTS5 :: LTSVersion
, gLTS6 :: LTSVersion
}

instance FromJSON LTSInfo where
  parseJSON = withObject "ltsinfo" $ \o -> do
    gLTS  <- o .: "lts"
    gLTS2 <- o .: "lts-2"
    gLTS3 <- o .: "lts-3"
    gLTS4 <- o .: "lts-4"
    gLTS5 <- o .: "lts-5"
    gLTS6 <- o .: "lts-6"
    return LTSInfo{..}

