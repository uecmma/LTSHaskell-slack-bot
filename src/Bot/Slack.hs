{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.Slack
  ( SlackMessage (..)
  , SlackPostInfo (..)
  , SlackPostData (..)
  , SlackConfig (..)
  , postToSlack
  , sendSlackMessage
  ) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BSL
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Network.HTTP.Conduit

data SlackMessage = SlackMessage
  { getIcon :: !Text
  , getText :: !Text
  } deriving (Show, Eq)

data SlackPostInfo = SlackPostInfo
  { getChannel  :: !Text
  , getUserName :: !Text
  } deriving (Show, Eq)

data SlackPostData = SlackPostData
  { getInfo    :: !SlackPostInfo
  , getMessage :: !SlackMessage
  } deriving (Show, Eq)

instance ToJSON SlackPostData where
  toJSON SlackPostData{..} = object
    [ "channel"    .= getChannel getInfo
    , "username"   .= getUserName getInfo
    , "text"       .= getText getMessage
    , "icon_emoji" .= getIcon getMessage
    ]

data SlackConfig = SlackConfig
  { webhookUrl :: !Text
  } deriving (Show, Eq)

getPostRequest ::
  MonadThrow m => [(BS.ByteString, BS.ByteString)] -> Text -> m Request
getPostRequest pdata = return . urlEncodedBody pdata <=< parseUrl . T.unpack

postToSlack :: SlackConfig -> SlackPostData -> IO BSL.ByteString
postToSlack c d = runResourceT $ do
  req <- getPostRequest
    [("payload", BSL.toStrict . encode $ d)]
    $ webhookUrl c
  manager <- liftIO $ newManager tlsManagerSettings
  res <- httpLbs req manager
  return $ responseBody res

sendSlackMessage :: MonadIO m => SlackConfig -> SlackPostInfo -> SlackMessage -> m ()
sendSlackMessage c i m = void $ liftIO $ postToSlack c $ SlackPostData i m
