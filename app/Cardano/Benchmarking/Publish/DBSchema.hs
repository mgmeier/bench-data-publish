{-# LANGUAGE OverloadedStrings #-}

module  Cardano.Benchmarking.Publish.DBSchema where

import           Data.ByteString.Char8              as BS (readFile)
import           Data.Functor.Contravariant         ((>$<))

import           Hasql.Connection                   as DB
import           Hasql.Encoders                     as Enc hiding (timestamp)
import           Hasql.Session                      as DB

import           Cardano.Benchmarking.Publish.Types

import           Paths_bench_data_publish


-- bootstraps schema onto a DB
-- destructive: drops a possible pre-existing schema
bootstrap :: Connection -> IO ()
bootstrap conn
  = do
    schema <- BS.readFile =<< getDataFileName "db/bench-data-schema.sql"
    let
      script :: DB.Session ()
      script = DB.sql schema

    _ <- DB.run script conn
    pure ()


-- encoder for table 'cluster_run'
encClusterRun :: Params MetaStub
encClusterRun
  =  (profile   >$< param (nonNullable text))
  <> (batch     >$< param (nonNullable text))
  <> (timestamp >$< param (nonNullable timestamptz))
