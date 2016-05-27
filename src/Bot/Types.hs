{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.Types
  ( LtsVersion (..)
  , Snapshots (..)
  , NightlyHaskellUpdate (..)
  , LtsHaskellUpdate (..)
  , LtsHaskellUpdates (..)
  , isLatestUpdate
  , isNightlyUpdate
  , hasEachLtsUpdates
  , hasLtsUpdates
  , checkUpdates
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Types     as AS
import qualified Data.Attoparsec.Text as AP
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IntMap
import           Data.Maybe           (fromMaybe, isJust)
import           Data.Semigroup
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time            (Day)
import           Safe                 (readMay)
import           TextShow

fromAttoparsec :: AP.Parser a -> Text -> AS.Parser a
fromAttoparsec p v =
  case AP.parseOnly (p <* AP.endOfInput) v of
    Left msg  -> fail msg
    Right res -> return res

data LtsVersion = LtsVersion Int Int deriving (Eq, Ord)

instance Show LtsVersion where
  show (LtsVersion v1 v2) = "lts-" ++ show v1 ++ "." ++ show v2

instance ToJSON LtsVersion where
  toJSON = String . T.pack . show

instance FromJSON LtsVersion where
  parseJSON (String s) = parser s
    where
      parser = fromAttoparsec $ LtsVersion
        <$> (AP.string "lts-" *> AP.decimal <* AP.char '.')
        <*> AP.decimal
  parseJSON _ = empty

data Snapshots = Snapshots
  { latestLts        :: !LtsVersion
  , snapshotsNightly :: !Day
  , snapshotsLts     :: !(IntMap LtsVersion)
  } deriving (Show, Eq)

instance FromJSON Snapshots where
  parseJSON = withObject "Snapshots" $ \o -> Snapshots
    <$> (o .: "lts")
    <*> (o .: "nightly" >>= parseNightly)
    <*> parseLtsVersions o
    where
      isLtsV = T.isPrefixOf "lts-"

      parseNightly :: Text -> AS.Parser Day
      parseNightly =
        (maybe (fail "Failed to parse nightly lts") return <<<) $
        T.stripPrefix "nightly-"
        >=> T.unpack >>> readMay

      parseLtsVersion :: Value -> AS.Parser (IntMap LtsVersion)
      parseLtsVersion =
        parseJSON >>> fmap
        (\v@(LtsVersion v1 _) -> IntMap.singleton v1 v)

      parseLtsVersions :: Object -> AS.Parser (IntMap LtsVersion)
      parseLtsVersions = HashMap.toList
        >>> filter (isLtsV . fst)
        >>> mapM (parseLtsVersion . snd)
        >>> fmap IntMap.unions

instance ToJSON Snapshots where
  toJSON Snapshots{..} = object $
    [ "lts" .= latestLts
    , "nightly" .= ("nightly-" <> T.pack (show snapshotsNightly))
    ] ++
    map (\(n, v) -> ("lts-" <> showt n) .= v)
    (IntMap.toList snapshotsLts)

data NightlyHaskellUpdate = NightlyHaskellUpdate
  { getBeforeNightlyDay :: Day
  , getAfterNightlyDay  :: Day
  } deriving (Eq)

instance Show NightlyHaskellUpdate where
  show (NightlyHaskellUpdate b a)
    = "nightly-" ++ show b ++ " -> nightly-" ++ show a

data LtsHaskellUpdate = LtsHaskellUpdate
  { getBeforeLtsVersion :: LtsVersion
  , getAfterLtsVersion  :: LtsVersion
  } deriving (Eq)

instance Show LtsHaskellUpdate where
  show (LtsHaskellUpdate b a) = show b ++ " -> " ++ show a

data LtsHaskellUpdates = LtsHaskellUpdates
  { getLatestUpdate   :: !(Maybe LtsHaskellUpdate)
  , getNightlyUpdate  :: !(Maybe NightlyHaskellUpdate)
  , getEachLtsUpdates :: !(IntMap LtsHaskellUpdate)
  } deriving (Show, Eq)

isLatestUpdate :: LtsHaskellUpdates -> Bool
isLatestUpdate = isJust . getLatestUpdate

isNightlyUpdate :: LtsHaskellUpdates -> Bool
isNightlyUpdate = isJust . getNightlyUpdate

hasEachLtsUpdates :: LtsHaskellUpdates -> Bool
hasEachLtsUpdates = (IntMap.empty /=) . getEachLtsUpdates

hasLtsUpdates :: LtsHaskellUpdates -> Bool
hasLtsUpdates ups
  =  isLatestUpdate ups
  || isNightlyUpdate ups
  || hasEachLtsUpdates ups

checkUpdates :: Snapshots -> Snapshots -> LtsHaskellUpdates
checkUpdates olds news = LtsHaskellUpdates
  Nothing
  Nothing
  IntMap.empty
