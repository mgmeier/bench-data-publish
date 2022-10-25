{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Data.Bool                                 (bool)
import           Data.ByteString.Char8                     as BS (pack,
                                                                  readFile)
import           Data.List                                 (isPrefixOf)
import           System.Directory
import           System.Directory.Extra                    (listDirectories)
import           System.Environment                        (getArgs)
import           System.FilePath

import           Data.Aeson
import           Hasql.Connection                          as DB (Connection)
import qualified Hasql.Session                             as DB

import           Cardano.Benchmarking.Publish.DBConnection
import           Cardano.Benchmarking.Publish.DBQueries
import           Cardano.Benchmarking.Publish.DBSchema
import           Cardano.Benchmarking.Publish.Types


dbSchema ::  DBSchema
dbSchema = DBSchema "api_prototype"

main :: IO ()
main = do
    uri <- getArgs >>= pure . \case
        "--pg-uri" : uri : _
            | "postgres" `isPrefixOf` uri -> uri
        _ -> ""

    if null uri
      then putStrLn "please specify postgres URI: --pg-uri postgres://<user>:<password>@<host>:<port>/<db_name>"
      else withDB (BS.pack uri) $ \conn -> do
        views <- bootstrap dbSchema conn
        putStrLn $ "bootstrapped schema '" ++ show dbSchema ++ "' onto: " ++ uri
        runMetas <- getCurrentDirectory >>= searchRuns . (</> "runs")
        putStrLn $ "found runs: " ++ show (length runMetas)
        mapM_ (storeRunToDB conn) (zip [1 ..] runMetas)
        putStrLn $ "exposed views to API: " ++ show views


storeRunToDB :: DB.Connection -> (Int, FilePath) -> IO ()
storeRunToDB conn (ix, metaFile)
  = loadRun metaFile >>= \case
      Left err -> logStr err
      Right run -> dbStoreRun dbSchema run `DB.run` conn >>= \case
        Left err' -> logStr $ show err'
        _         -> putStrLn $ show ix ++ " -- " ++ show (metaStub run) ++ ": stored"
  where
    logStr str = putStrLn $ show ix ++ " -- " ++ metaFile ++ ": " ++ str

searchRuns :: FilePath -> IO [FilePath]
searchRuns targetDir
  = do
    subDirs <- listDirectories targetDir
    filterM doesFileExist $ map (</> "meta.json") subDirs

-- given a path to its meta.json, loads all pertaining data into a ClusterRun
loadRun :: FilePath -> IO (Either String ClusterRun)
loadRun metaFile
  = do
    runMeta <- BS.readFile metaFile
    case eitherDecodeStrict' runMeta of
        Left e -> pure $ Left e
        Right metaStub -> do
          runBlockProp <- tryReadFile $ analysis </> "blockprop.json"
          runClusterPerf <- tryReadFile $ analysis </> "clusterperf.json"
          pure $! Right $! ClusterRun{..}
  `catch`
    \(SomeException e) -> pure (Left $ show e)
  where
    analysis = takeDirectory metaFile </> "analysis"
    tryReadFile f
      = doesFileExist f >>= bool (pure Nothing) (Just <$> BS.readFile f)
