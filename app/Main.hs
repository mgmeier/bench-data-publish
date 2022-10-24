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
import           System.Environment                        (getArgs)
import           System.FilePath

import           Data.Aeson
import           Hasql.Connection                          as DB (Connection)
import qualified Hasql.Session                             as DB

import           Cardano.Benchmarking.Publish.DBConnection
import           Cardano.Benchmarking.Publish.DBQueries    (storeRun)
import           Cardano.Benchmarking.Publish.DBSchema
import           Cardano.Benchmarking.Publish.Types

main :: IO ()
main = do
    uri <- getArgs >>= pure . \case
        "--pg-uri" : uri : _
            | "postgres" `isPrefixOf` uri -> uri
        _ -> ""

    if null uri
      then putStrLn "please specify postgres URI: --pg-uri postgres://<user>:<password>@<host>:<port>/<db_name>"
      else withDB (BS.pack uri) $ \conn -> do
        bootstrap conn
        putStrLn $ "bootstrapped schema 'api_prototype' onto: " ++ uri
        runMetas <- getCurrentDirectory >>= searchRuns . (</> "runs")
        putStrLn $ "found runs: " ++ show (length runMetas)
        mapM_ (storeRunToDB conn) runMetas

storeRunToDB :: DB.Connection -> FilePath -> IO ()
storeRunToDB conn runMeta
  = loadRun runMeta >>= \case
      Left err -> cry err
      Right run -> storeRun run `DB.run` conn >>= \case
        Left err' -> cry (show err')
        _         -> pure ()
  where
    cry errStr = putStrLn $ runMeta ++ ": " ++ errStr

searchRuns :: FilePath -> IO [FilePath]
searchRuns targetDir
  = do
    subDirs <- listDirectory targetDir
    runDirs <- filterM doesDirectoryExist $ map (targetDir </>) subDirs
    filterM doesFileExist $ map (</> "meta.json") runDirs

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
