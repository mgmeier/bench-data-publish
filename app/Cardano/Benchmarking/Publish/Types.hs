{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module  Cardano.Benchmarking.Publish.Types where

import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import           Data.Time.Clock.System
import           GHC.Generics           (Generic)

import           Data.Aeson

-- we extract only very few attributes from meta.json to perform
-- a necessary minimum of data normalization to describe a run
data MetaStub
  = MetaStub {
      profile   :: Text
    , batch     :: Text
    , timestamp :: UTCTime
    }
    deriving (Show, Generic)

instance FromJSON MetaStub where
    parseJSON = withObject "MetaStub" $ \o_ -> do
      o <- o_ .: "meta"
      MetaStub
        <$> o .: "profile"
        <*> o .: "batch"
        <*> (systemToUTCTime <$> o .: "timestamp")

test :: FilePath -> IO ()
test metaJson = do
    metaStub :: Either String MetaStub <-
        eitherDecodeFileStrict metaJson
    print metaStub
