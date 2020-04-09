{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Database.Persist.Sql
import Data.Maybe
import Data.Text as T
import Text.Read as R

getHomeR :: Handler Html
getHomeR = do
    sessId <- lookupSession "_ID"
    let tmp = T.unpack $ fromJust sessId
    let tmp2 = R.read tmp ::Int64
    let id' = toSqlKey tmp2
    let deviceId = toSqlKey 0
    seldev <- runDB $ selectList [DeviceUserId ==. id', DeviceId ==. deviceId] [] 
    dev <- runDB $ selectList [DeviceUserId ==. id'] [Asc DeviceId]
    value <- runDB $ selectList [ValuesDeviceId ==. deviceId] [Asc ValuesTime]
    defaultLayout $ do
        setTitle "Welcome to Ročníkáč"
        $(widgetFile "homepage")
