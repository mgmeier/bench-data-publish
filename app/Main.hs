{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString.Char8                     as BS (pack)
import           Data.List                                 (isPrefixOf)
import           System.Environment                        (getArgs)

import           Cardano.Benchmarking.Publish.DBConnection
import           Cardano.Benchmarking.Publish.DBSchema


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
