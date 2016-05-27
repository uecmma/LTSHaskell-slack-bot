{-# LANGUAGE OverloadedStrings #-}

module Bot.Run where

import Bot.Types
import Control.Concurrent (threadDelay)
import System.IO (stdin, stdout)
import Control.Monad.Fix (fix)
import Data.Aeson
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Network.HTTP.Conduit
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Resource (runResourceT, ResourceT, MonadResource)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (toStrict, fromStrict)

-- | parseUrl function with Post Data!
parseUrlPost :: MonadThrow m => [(BS.ByteString,BS.ByteString)] -> String -> m Request
parseUrlPost post = return . urlEncodedBody post <=< parseUrl

-- | To get LTS version info with URL of JSON.
getLTS :: MonadIO m => URL -> m LTSInfo
getLTS url = do
  Just ltsi <- decode <$> simpleHttp url
  return ltsi

-- | send slack message to our slack!
sendMessage :: URL -> SlackMessage -> IO ()
sendMessage url sm = runResourceT $ do
  req <- parseUrlPost
    [("payload", toStrict . encode $ sm)] url
  manager <- liftIO $ newManager conduitManagerSettings
  res <- http req manager
  responseBody res $$+- CB.sinkHandle stdout


-- | ikkaidake, tls check and send message to slack.
onece :: URL -> URL -> LTSVersion -> IO LTSVersion
onece ltsURL slackURL old = do
  new <- gLTS <$> getLTS ltsURL
  when (old /= new)
    (sendMessage slackURL SlackMessage {
      getChannel = "haskell"
    , getUserName = "lts-bot"
    , getText = "lts の ばーじょんが あがったぞ！,\n" ++ old ++ " ---> " ++ new
    , getIconEmoji = ":abe:" })
  putStrLn $ old ++ " ---> " ++ new
  threadDelay $ 10 * 60 * 1000 * 1000 -- maikuro byou
  return new


-- | main loop
botMainLoop :: URL -> URL -> IO ()
botMainLoop ltsURL slackURL = do
  now <- gLTS <$> getLTS ltsURL
  fix (onece ltsURL slackURL >=>) now
