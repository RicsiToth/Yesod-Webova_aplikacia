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
module Handler.SendDataTime where

import Import
import Data.Time.Format
import Data.Text
import Data.Text.Time

getSendDataTimeR :: Text -> DeviceId -> Int -> Text -> Handler Html
getSendDataTimeR uuid id' value time = do
    valid <- validation id' uuid
    if(valid)
        then do
            let time' = parseISODateTime time
            _ <- runDB $ insert $ Values value time' id'
            defaultLayout [whamlet|<p>OK|]
        else defaultLayout [whamlet|<p>NIE|]

postSendDataTimeR :: Text -> DeviceId -> Int -> Text -> Handler Html
postSendDataTimeR uuid id' value time = getSendDataTimeR uuid id' value time

validation :: DeviceId -> Text -> Handler Bool
validation id' uuid = do
  results <- runDB $ selectList [DeviceId ==. id', DeviceUuid ==. uuid] []
  case results of
    [] -> return False
    _  -> return True
