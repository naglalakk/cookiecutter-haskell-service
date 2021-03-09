module Models where

import Database.Persist.Sql
  ( SqlPersistT,
    runMigration,
  )
import Model.User (migrateUser)

doMigrations :: SqlPersistT IO ()
doMigrations = do
  runMigration migrateUser
