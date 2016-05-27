{-# LANGUAGE RecordWildCards #-}

module Bot.Options (
  BotOptions(..)
, getBotOptions
) where

import Bot.Types
import Options.Applicative

data BotOptions = BotOptions {
  optJsonURL :: URL
}

botOptionsParser :: Parser BotOptions
botOptionsParser = BotOptions
  <$> option str (
        long "webhook_url"
     <> metavar "URL"
     <> help "Webhook URL")

allBotInfo :: ParserInfo BotOptions
allBotInfo = info
  (helper <*> botOptionsParser)
  (fullDesc
    <> progDesc "LTSHaskell Slack Bot"
    <> header   "notification updating of LTSHaskell")

getBotOptions :: IO BotOptions
getBotOptions = execParser allBotInfo
