{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module  Cardano.Benchmarking.Publish.DBQueries
        ( storeRun
        ) where

import           Data.ByteString.Char8                 (ByteString)

import           Hasql.Decoders                        as Dec
import           Hasql.Session                         hiding (sql)
import           Hasql.Statement

import           Cardano.Benchmarking.Publish.DBSchema
import           Cardano.Benchmarking.Publish.Types


getRunId :: Statement MetaStub (Maybe Int)
getRunId
  = Statement sql encClusterRun (rowMaybe decInt4) False
  where
    sql = "SELECT id FROM api_prototype.cluster_run WHERE run_profile=$1 AND run_batch=$2 AND run_at=$3"

createRunId :: Statement MetaStub Int
createRunId
  = Statement sql encClusterRun (singleRow decInt4) False
  where
    sql = "INSERT INTO api_prototype.cluster_run (run_profile, run_batch, run_at)\
          \ VALUES ($1,$2,$3)\
          \ ON CONFLICT ON CONSTRAINT un_run_profile DO NOTHING\
          \ RETURNING id"

setMeta :: Statement (Int, ByteString) ()
setMeta
  = Statement sql encRunInfo Dec.noResult False
  where
    sql = "INSERT INTO api_prototype.run_info VALUES ($1,$2)\
          \ ON CONFLICT ON CONSTRAINT un_info_run_id DO UPDATE\
          \ SET meta=$1"

decInt4 :: Row Int
decInt4 = fromIntegral <$> (column . nonNullable) Dec.int4



storeRun :: ClusterRun -> Session ()
storeRun ClusterRun{..}
  = do
    runId <- statement metaStub getRunId >>=
        maybe (statement metaStub createRunId) pure
    statement (runId, runMeta) setMeta
