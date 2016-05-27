{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as Text
import Bot.Run (botMainLoop)
import Bot.Config
import Bot.Options

main :: IO ()
main = do
  bo <- getBotOptions
  botMainLoop
    (Text.unpack . head $ defaultSnapshotsURLs)
    (optJsonURL bo)
