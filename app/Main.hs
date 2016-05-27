{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Maybe             (isNothing)
import qualified Data.Text              as T
import           System.Cron

import           Bot.Config
import           Bot.Message
import           Bot.Run
import           Bot.Slack

waiting :: Int -> IO ()
waiting n = forever $ threadDelay n

main :: IO ()
main = do
  opts <- getBotOptions
  url <- T.pack <$> case optsWebhookUrl opts of
    Just v -> return v
    _      -> fail "Must be set webhook url"
  snaps <- getLtsSnapshots_ url
  let stmsnaps = newTVar snaps
  tids <- execSchedule $
    addJob (updateSlackCronJob stmsnaps $ sendMsg url) "* * * * *"
  waiting 1000000
  where
    slackConf = SlackConfig
    slackInfo = SlackPostInfo "haskell" "ltshaskell-bot"
    slackMessage = SlackMessage ":haskell:"

    sendMsg url info = sendSlackMessage
      (slackConf url)
      slackInfo
      (slackMessage $ defaultMessage info)
