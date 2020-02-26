{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.RemoveDevice where

import Import
import Database.Persist.Sql
import Data.Maybe
import Data.Text as T
import Text.Read as R

getRemoveDeviceR :: DeviceId -> Handler Html
getRemoveDeviceR devId = do
    sessId <- lookupSession "_ID"
    let tmp = T.unpack $ fromJust sessId
    let tmp2 = R.read tmp ::Int64
    let uId = toSqlKey tmp2
    valid <- validation uId devId
    if(valid)
        then do
            _ <- runDB $ deleteWhere [ValuesDeviceId ==. devId]
            _ <- runDB $ delete devId
            redirect HomeR
        else redirect HomeR --Možno dodatočnú správu

postRemoveDeviceR :: DeviceId -> Handler Html
postRemoveDeviceR devId = getRemoveDeviceR devId

validation :: UserId -> DeviceId -> Handler Bool
validation uId devId = do
  results <- runDB $ selectList [DeviceUserId ==. uId, DeviceId ==. devId] []
  case results of
    [] -> return False
    _  -> return True
