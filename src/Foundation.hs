{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool)
import Database.Persist.TH
  ( mkMigrate
  , mkPersist
  , persistFileWith
  , share
  , sqlSettings
  )
import Yesod
  ( Yesod
  , YesodPersist(YesodPersistBackend, runDB)
  , getYesod
  , mkYesodData
  , parseRoutesFile
  , renderRoute
  )

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

data App =
  App
    { connectionPool :: ConnectionPool
    }

mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

instance Yesod App

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = getYesod >>= runSqlPool action . connectionPool
