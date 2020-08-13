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
import Data.Text
import Text.Read
import Database.Persist.Sql
import Data.Text.Time

getSendDataTimeR :: Text -> Text -> Text -> Handler Html
getSendDataTimeR uuid value time = do
    (valid, id') <- validation uuid
    if(valid)
        then do
            let time' = parseISODateTime time
            let value' = read $ Data.Text.unpack value
            _ <- runDB $ insert $ Values value' time' id'
            defaultLayout [whamlet|<p>OK|]
        else defaultLayout [whamlet|<p>NIE|]

postSendDataTimeR :: Text -> Text -> Text -> Handler Html
postSendDataTimeR uuid value time = getSendDataTimeR uuid value time

validation :: Text -> Handler (Bool, DeviceId)
validation uuid = do
  results <- runDB $ getBy $ UniqueUuid uuid
  case results of
    Nothing -> return (False, toSqlKey 0)
    Just(Entity deviceId _) -> return (True, deviceId)
