{-# LANGUAGE OverloadedStrings #-}

module Bot.Config where

import           Data.Text            (Text)
import           Network.HTTP.Conduit
import           Options.Applicative

defaultSnapshotsURLs :: [Text]
defaultSnapshotsURLs =
  [ "https://www.stackage.org/download/snapshots.json"
  , "https://s3.amazonaws.com/haddock.stackage.org/snapshots.json"
  ]

data BotOptions = BotOptions
  { optsWebhookUrl   :: !(Maybe String)
  , optsDelayTime    :: !(Maybe Int)
  , optsConfigPath   :: !(Maybe String)
  , optsSnapshotsUrl :: !(Maybe String)
  } deriving (Show, Eq)

botOptionsParser :: Parser BotOptions
botOptionsParser = BotOptions
  <$> webHookUrlP
  <*> delayTimeP
  <*> configPathP
  <*> snapshotsUrlP
  where
    webHookUrlP = optional
      $  option str
      $  long "url"
      <> long "webhook-url"
      <> help "WebHook URL for Slack Incoming"
      <> metavar "URL"

    delayTimeP = optional
      $  option auto
      $  short 't'
      <> long "delay-time"
      <> help "Delay time (seconds)"
      <> metavar "TIME"

    configPathP = optional
      $  option str
      $  short 'c'
      <> long "config"
      <> help "config file"
      <> metavar "FILE"

    snapshotsUrlP = optional
      $  option str
      $  long "snapshots-url"
      <> help "snapshots url"
      <> metavar "URL"

allBotInfo :: ParserInfo BotOptions
allBotInfo = info
  (helper <*> botOptionsParser)
  $  fullDesc
  <> progDesc "LTSHaskell Slack Bot"
  <> header   "notification updating of LTSHaskell"

getBotOptions :: IO BotOptions
getBotOptions = execParser allBotInfo
