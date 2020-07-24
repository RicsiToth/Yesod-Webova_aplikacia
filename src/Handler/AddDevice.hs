{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.AddDevice where

import Import
import Database.Persist.Sql
import Data.Maybe
import Data.Text as T
import Text.Read as R
import Data.GUID

data NewDevice = NewDevice{ name :: Text }

getAddDeviceR :: Handler Html
getAddDeviceR = do
  (widget, enctype) <- generateFormPost newDeviceForm
  defaultLayout $ do
    setTitle "Add Device"
    $(widgetFile "add-device")

postAddDeviceR :: Handler Html
postAddDeviceR = do
    ((result, _), _) <- runFormPost newDeviceForm
    case result of
        FormSuccess nu -> do
            sessId <- lookupSession "_ID"
            let tmp = T.unpack $ fromJust sessId
            let tmp2 = R.read tmp ::Int64
            let id' = toSqlKey tmp2
            uuid <- liftIO $ genText
            _ <- runDB $ insert $ Device (name nu) uuid "1" id'
            redirect AddDeviceR
        _ -> do
            setMessage "Something went wrong"
            redirect AddDeviceR

newDeviceForm :: Form NewDevice
newDeviceForm = renderDivs $ NewDevice
              <$> areq textField "Name" Nothing