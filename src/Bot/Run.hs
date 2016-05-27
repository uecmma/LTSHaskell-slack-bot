module Bot.Run
  ( getLtsSnapshots
  , getLtsSnapshots_
  , checkLtsHaskellUpdates
  , updateSlackCronJob
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Network.HTTP.Conduit

import           Bot.Config
import           Bot.Message
import           Bot.Types

getLtsSnapshots :: MonadIO m => Text -> m (Maybe Snapshots)
getLtsSnapshots url = decode <$> simpleHttp (T.unpack url)

getLtsSnapshots_ :: MonadIO m => Text -> m Snapshots
getLtsSnapshots_ url = do
  msnaps <- getLtsSnapshots url
  case msnaps of
    Just snaps -> return snaps
    _          -> fail "Failed to get snapshots"

checkLtsHaskellUpdates :: MonadIO m => Snapshots -> m (Snapshots, LtsHaskellUpdates)
checkLtsHaskellUpdates snaps = do
  nsnaps <- getLtsSnapshots_ $ head defaultSnapshotsURLs
  return (nsnaps, checkUpdates snaps nsnaps)

updateSlackCronJob :: MonadIO m => STM (TVar Snapshots) -> (LtsHaskellUpdateInfo -> m ()) -> m ()
updateSlackCronJob stmsnaps sendMsg = do
  snaps <- liftIO $ atomically $ stmsnaps >>= readTVar
  (newsnaps, updates) <- checkLtsHaskellUpdates snaps
  liftIO $ putStrLn $ "trace: " ++ show newsnaps
  liftIO $ atomically $ stmsnaps >>= flip writeTVar newsnaps
  fromMaybe (return ()) $ do
    info <- toLatestInfo updates
    return $ liftIO (putStrLn "trace: sending to slack...") >> sendMsg info
