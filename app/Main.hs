{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Data.Bool                                 (bool)
import           Data.ByteString.Char8                     as BS (empty, pack,
                                                                  readFile)
import           System.Directory
import           System.Directory.Extra                    (listDirectories)
import           System.Environment                        (lookupEnv)
import           System.FilePath
import           System.Posix.User                         (getLoginName)
import           Text.Printf

import           Data.Aeson
import           Hasql.Connection                          as DB (Connection,
                                                                  Settings,
                                                                  settings)
import qualified Hasql.Session                             as DB

import           Cardano.Benchmarking.Publish.DBConnection
import           Cardano.Benchmarking.Publish.DBQueries
import           Cardano.Benchmarking.Publish.DBSchema
import           Cardano.Benchmarking.Publish.Types
import           Command


main :: IO ()
main
  = do
    conf <- parseCommandLine
    dbSettings <- getDBSettings (appDB conf)
    eval conf dbSettings

eval :: Config -> DB.Settings -> IO ()
eval Config{..} dbSettings
    = case appCommand of

      Bootstrap anonRole
        | not appForce -> putStrLn "'bootstrap' requires -f (force); it is a destructive operation"
        | otherwise -> withDB dbSettings $ \conn -> do
          bootstrap dbSchema (BS.pack anonRole) conn >>= \case
            Left err -> print err
            Right views -> putStrLn $
              "successfully bootstrapped schema: '" ++ appDBSchema ++ "'\n\
              \views exposed to API (role '" ++ anonRole ++ "'): " ++ show views

      ImportAll targetDir -> do
        runMetas <- searchRuns targetDir
        putStrLn $ "found runs: " ++ show (length runMetas)
        unless (null runMetas) $
          withDB dbSettings $ \conn ->
            storeRunsToDB dbSchema conn runMetas

      Import targetFile ->
        withDB dbSettings $ \conn ->
          storeRunsToDB dbSchema conn [targetFile]

      List ->
        withDB dbSettings $ \conn ->
          dbGetRuns dbSchema `DB.run` conn >>= \case
            Left err -> putStrLn $ "List: -- ERROR: " ++ show err
            Right runs -> do
              forM_ runs $ \(ix, meta, publ) ->
                printf "%3i %s -- %s\n" ix (published publ) (show meta)
              printf "----\n%i runs\n" (length runs)

      cmd -> putStrLn $ "command not yet implemented: " ++ show cmd

  where
    dbSchema = DBSchema (BS.pack appDBSchema)
    published p = bool '-' '+' p : "published"

storeRunsToDB :: DBSchema -> DB.Connection -> [FilePath] -> IO ()
storeRunsToDB dbSchema conn metaFiles
  = do
    created <- forM metaFiles $ \metaFile -> do
      putStr $ "storing: " ++ metaFile ++ " -- "
      loadRun metaFile >>= \case
        Left err -> errorStore err
        Right run -> dbStoreRun dbSchema run `DB.run` conn
          >>= either
            (errorStore . show)
            (\created -> putStrLn (bool "UPDATED" "CREATED" created) >> pure created)

    -- for any change to the run list itself (not the associated results)
    -- we need to refresh the materialized view
    when (or created) $
      void $ dbRefreshView dbSchema `DB.run` conn
  where
    errorStore str = putStrLn ("ERROR: " ++ str) >> pure False

searchRuns :: FilePath -> IO [FilePath]
searchRuns targetDir
  = do
    subDirs <- listDirectories targetDir
    filterM doesFileExist $ map (</> "meta.json") subDirs

-- given a path to its directory or meta.json, loads all pertaining data into a ClusterRun
loadRun :: FilePath -> IO (Either String ClusterRun)
loadRun metaFile_
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
    metaFile
      | takeFileName metaFile_ == "meta.json" = metaFile_
      | otherwise = metaFile_ </> "meta.json"
    analysis = takeDirectory metaFile </> "analysis"
    tryReadFile f
      = doesFileExist f >>= bool (pure Nothing) (Just <$> BS.readFile f)

getDBSettings :: DBCredentials -> IO DB.Settings
getDBSettings NoCreds
  = maybe BS.empty BS.pack <$> lookupEnv envVarDBURI
getDBSettings (PostgresURI uri)
  = pure $ BS.pack uri
getDBSettings DBCreds{..}
  = do
    envUser <- BS.pack <$> getLoginName
    envPass <- maybe BS.empty BS.pack <$> lookupEnv envVarDBPass
    pure $ DB.settings
      (maybe "localhost" BS.pack dbHost)
      (maybe 5432 fromIntegral dbPort)
      (maybe envUser BS.pack dbUser)
      (maybe envPass BS.pack dbPass)
      (BS.pack dbName)
