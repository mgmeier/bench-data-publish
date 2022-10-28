{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module  Cardano.Benchmarking.Publish.DBSchema where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except.Extra
import           Data.ByteString.Char8              as BS (ByteString, null,
                                                           readFile, unpack)
import           Data.Functor.Contravariant         ((>$<))
import           Data.Text                          as T (Text, intercalate,
                                                          snoc)
import           Data.Text.Encoding                 as T (decodeLatin1,
                                                          encodeUtf8)
import           Data.Tuple.Extra

import           Hasql.Connection                   as DB
import           Hasql.Decoders                     as Dec hiding (timestamp)
import           Hasql.Encoders                     as Enc hiding (timestamp)
import           Hasql.Session                      as DB
import           Hasql.Statement                    as DB (Statement (Statement))

import           Cardano.Benchmarking.Publish.Types

import           Paths_bench_data_publish


newtype DBSchema = DBSchema BS.ByteString

instance Show DBSchema where
  show (DBSchema s) = BS.unpack s


-- bootstraps schema onto a DB
-- and grants read access of all views defined to the specified role
-- destructive: drops a possible pre-existing schema with all data
bootstrap :: DBSchema -> ByteString -> Connection -> IO (Either DB.QueryError [Text])
bootstrap (DBSchema schemaName) anonRole conn
  = runExceptT $ do
      schema <- liftIO $
        BS.readFile =<< getDataFileName "db/bench-data-schema.sql"
      let
        script :: DB.Session ()
        script = DB.sql (preamble <> schema)

      _ <- liftDBRun script
      views <- liftDBRun getViews
      let
        sch = T.decodeLatin1 schemaName `T.snoc` '.'
        commaSep = T.intercalate "," $ (sch <>) <$> views
        commaSepBS = T.encodeUtf8 commaSep

      _ <- liftDBRun (grant commaSepBS)
      pure views
  where
    liftDBRun session
      = hoistEither =<< liftIO (DB.run session conn)

    preamble :: ByteString
    preamble =
      "DROP SCHEMA IF EXISTS " <> schemaName <> " CASCADE;\n\
      \CREATE SCHEMA " <> schemaName <> ";\n\
      \COMMENT ON SCHEMA " <> schemaName <> " IS 'This schema provides benchmarking cluster run data';\n\
      \SET search_path TO " <> schemaName <> ";\n"

    getViews :: DB.Session [Text]
    getViews = statement () $
      Statement queryViewNames Enc.noParams (rowList decText) False
      where
        queryViewNames = 
          "SELECT viewname FROM pg_catalog.pg_views WHERE schemaname='" <> schemaName <> "'\
          \ UNION SELECT matviewname FROM pg_catalog.pg_matviews WHERE schemaname='" <> schemaName <> "'"

    grant :: ByteString -> DB.Session ()
    grant commaSep =
      let toAnon = " TO " <> anonRole <> ";\n"
      in DB.sql $
        "GRANT USAGE ON SCHEMA " <> schemaName <> toAnon
        <> if BS.null commaSep then mempty else "GRANT SELECT ON " <> commaSep <> toAnon


-- encoder for table 'cluster_run'
encClusterRun :: Params MetaStub
encClusterRun
  =  (profile   >$< param (Enc.nonNullable Enc.text))
  <> (batch     >$< param (Enc.nonNullable Enc.text))
  <> (timestamp >$< param (Enc.nonNullable Enc.timestamptz))

-- encoder for table 'run_info'
encRunInfo :: Params (Int, ByteString)
encRunInfo
  =  (snd                >$< param (Enc.nonNullable Enc.jsonbBytes))
  <> (fromIntegral . fst >$< param (Enc.nonNullable Enc.int4))

-- encoder for table 'run_result'
encRunResult :: Params (Int, Maybe ByteString, Maybe ByteString)
encRunResult
  =  (snd3                >$< param (Enc.nullable Enc.jsonbBytes))
  <> (thd3                >$< param (Enc.nullable Enc.jsonbBytes))
  <> (fromIntegral . fst3 >$< param (Enc.nonNullable Enc.int4))

-- decoder for table 'cluster_run' (including id and published status)
decClusterRun :: Row (Int, MetaStub, Bool)
decClusterRun
  = (,,) <$> decInt4 <*> decMetaStub <*> decBool


-- convenience definitions

decInt4 :: Row Int
decInt4 = fromIntegral <$> (column . Dec.nonNullable) Dec.int4

decText :: Row Text
decText = (column . Dec.nonNullable) Dec.text

decBool :: Row Bool
decBool = (column . Dec.nonNullable) Dec.bool

decMetaStub :: Row MetaStub
decMetaStub
  = MetaStub
    <$> decText
    <*> decText
    <*> (column . Dec.nonNullable) Dec.timestamptz
