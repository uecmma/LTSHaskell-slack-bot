module Bot.Run
  ( getLtsSnapshots
  ) where

import           Control.Monad.Trans
import           Data.Aeson
import           Network.HTTP.Conduit

import           Bot.Types

getLtsSnapshots :: MonadIO m => String -> m (Maybe Snapshots)
getLtsSnapshots url = decode <$> simpleHttp url
