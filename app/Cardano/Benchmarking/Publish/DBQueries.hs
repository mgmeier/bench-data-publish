{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module  Cardano.Benchmarking.Publish.DBQueries
        ( dbStoreRun
        , dbRefreshView
        ) where

import           Data.ByteString.Char8                 (ByteString)

import           Hasql.Decoders                        as Dec
import qualified Hasql.Session                         as DB
import           Hasql.Statement

import           Cardano.Benchmarking.Publish.DBSchema
import           Cardano.Benchmarking.Publish.Types


getRunId :: ByteString -> Statement MetaStub (Maybe Int)
getRunId schema
  = Statement sql encClusterRun (rowMaybe decInt4) False
  where
    sql = "SELECT id FROM " <> schema <> ".cluster_run\
          \ WHERE run_profile=$1 AND run_batch=$2 AND run_at=$3"

createRunId :: ByteString -> Statement MetaStub Int
createRunId schema
  = Statement sql encClusterRun (singleRow decInt4) False
  where
    sql = "INSERT INTO " <> schema <> ".cluster_run (run_profile, run_batch, run_at)\
          \ VALUES ($1,$2,$3)\
          \ ON CONFLICT ON CONSTRAINT un_run_profile DO NOTHING\
          \ RETURNING id"

setMeta :: ByteString -> Statement (Int, ByteString) ()
setMeta schema
  = Statement sql encRunInfo Dec.noResult False
  where
    sql = "INSERT INTO " <> schema <> ".run_info VALUES ($1,$2)\
          \ ON CONFLICT ON CONSTRAINT un_info_run_id DO UPDATE\
          \ SET meta=$1"

setResult :: ByteString -> Statement (Int, Maybe ByteString, Maybe ByteString) ()
setResult schema
  = Statement sql encRunResult Dec.noResult False
  where
    sql = "INSERT INTO " <> schema <> ".run_result VALUES ($1,$2,$3)\
          \ ON CONFLICT ON CONSTRAINT un_result_run_id DO UPDATE\
          \ SET blockprop=$1, clusterperf=$2"


dbStoreRun :: DBSchema -> ClusterRun -> DB.Session ()
dbStoreRun (DBSchema schemaName) ClusterRun{..}
  = do
    runId <- DB.statement metaStub (getRunId schemaName) >>=
        maybe (DB.statement metaStub (createRunId schemaName)) pure
    DB.statement (runId, runMeta) (setMeta schemaName)
    DB.statement (runId, runBlockProp, runClusterPerf) (setResult schemaName)

dbRefreshView :: DBSchema -> DB.Session ()
dbRefreshView (DBSchema schemaName)
  = DB.sql $ "REFRESH MATERIALIZED VIEW " <> schemaName <> ".run;"
