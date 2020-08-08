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
module Handler.SendData where

import Import
import Data.Time.Clock
import Data.Text
import Text.Read
import Database.Persist.Sql

getSendDataR :: Text -> Text -> Handler Html
getSendDataR uuid value = do
    (valid, id') <- validation uuid
    if(valid)
        then do
            time <- liftIO myTime
            let value' = read $ Data.Text.unpack value
            _ <- runDB $ insert $ Values value' time id'
            defaultLayout [whamlet|<p>OK|]
        else defaultLayout [whamlet|<p>NIE|]

postSendDataR :: Text -> Text -> Handler Html
postSendDataR uuid value = getSendDataR uuid value

validation :: Text -> Handler (Bool, DeviceId)
validation uuid = do
  results <- runDB $ getBy $ UniqueUuid uuid
  case results of
    Nothing -> return (False, toSqlKey 0)
    Just(Entity deviceId _) -> return (True, deviceId)

myTime :: IO UTCTime
myTime = do
  utc <- getCurrentTime
  return $ addUTCTime (120*60) utc