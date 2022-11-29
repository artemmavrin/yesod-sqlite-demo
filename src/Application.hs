{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Application
  ( main
  ) where

import Conduit (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Text (Text)
import Database.Persist.Sql (runMigration, runSqlPool)
import Database.Persist.Sqlite (withSqlitePool)
import Yesod (mkYesodDispatch, warp)

import Foundation
import Handler

mkYesodDispatch "App" resourcesApp

dbName :: Text
dbName = "env.db"

openConnectionCount :: Int
openConnectionCount = 10

main :: IO ()
main = do
  putStrLn "Serving application on http://localhost:3000"
  runStderrLoggingT $
    withSqlitePool dbName openConnectionCount $ \pool ->
      liftIO $ do
        runResourceT $ flip runSqlPool pool $ runMigration migrateAll
        warp 3000 $ App pool
