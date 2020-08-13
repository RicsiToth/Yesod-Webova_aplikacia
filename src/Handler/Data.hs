{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Handler.Data where

import Import
import Yesod.Core.Json (returnJson)
import Database.Persist.Sql

getDataR :: Text -> Handler Value
getDataR uuid = do
    devId <- getId uuid
    tmp <- runDB $ selectList [ValuesDeviceId ==. devId] [Asc ValuesTime]
    returnJson (map entityVal tmp)

getId :: Text -> Handler DeviceId
getId uuid = do
  results <- runDB $ getBy $ UniqueUuid uuid
  case results of
    Nothing -> return (toSqlKey 0)
    Just(Entity deviceId _) -> return deviceId