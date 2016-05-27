{-# LANGUAGE OverloadedStrings #-}

module Bot.Config where

import           Data.Text            (Text)
import           Network.HTTP.Conduit

defaultSnapshotsURLs :: [Text]
defaultSnapshotsURLs =
  [ "https://www.stackage.org/download/snapshots.json"
  , "https://s3.amazonaws.com/haddock.stackage.org/snapshots.json"
  ]
