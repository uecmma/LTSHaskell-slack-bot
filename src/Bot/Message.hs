{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Bot.Message where

import           Data.IntMap    (IntMap)
import qualified Data.IntMap    as IntMap
import           Data.Semigroup
import           Data.Text      (Text)
import qualified Data.Text      as T

import           Bot.Types

data MessageLang
  = LanguageEnUs
  | LanguageJaJp
  deriving (Show, Eq, Ord, Enum)

data MessageTheme
  = NormalTheme
  | DetailTheme
  | PrettyTheme
  deriving (Show, Eq, Ord, Enum)

data LtsHaskellType
  = LtsHaskell Int
  | LatestLtsHaskell
  | NightlyLtsHaskell
  deriving (Show, Eq, Ord)

data LtsHaskellUpdateInfo = LtsHaskellUpdateInfo
  { getLtsHaskellType :: !LtsHaskellType
  , getBeforeVersion  :: !Text
  , getAfterVersion   :: !Text
  , getDetailUrl      :: !Text
  }

toLatestInfo :: LtsHaskellUpdates -> Maybe LtsHaskellUpdateInfo
toLatestInfo updates = do
  info <- getLatestUpdate updates
  let befver = T.pack . show $ getBeforeLtsVersion info
  let aftver = T.pack . show $ getAfterLtsVersion info
  return $ LtsHaskellUpdateInfo
    LatestLtsHaskell
    befver aftver
    $ "https://www.stackage.org/" <> aftver

toNightlyInfo :: LtsHaskellUpdates -> Maybe LtsHaskellUpdateInfo
toNightlyInfo updates = do
  info <- getNightlyUpdate updates
  let befver = "nightly-" <> T.pack (show $ getBeforeNightlyDay info)
  let aftver = "nightly-" <> T.pack (show $ getAfterNightlyDay info)
  return $ LtsHaskellUpdateInfo
    NightlyLtsHaskell
    befver aftver
    $ "https://www.stackage.org/" <> aftver

toLtsInfos :: LtsHaskellUpdates -> IntMap LtsHaskellUpdateInfo
toLtsInfos updates = IntMap.fromList $ do
  (v, info) <- IntMap.toList $ getEachLtsUpdates updates
  let befver = T.pack . show $ getBeforeLtsVersion info
  let aftver = T.pack . show $ getAfterLtsVersion info
  return . (v,) $ LtsHaskellUpdateInfo
    (LtsHaskell v)
    befver aftver
    $ "https://www.stackage.org/" <> aftver

defaultMessage :: LtsHaskellUpdateInfo -> Text
defaultMessage LtsHaskellUpdateInfo{..} =
  "LTS Haskell updated! :tada:   "
  <> "(" <> getBeforeVersion <> " -> " <> getAfterVersion <> ")\n"
  <> "For more information, see " <> getDetailUrl
