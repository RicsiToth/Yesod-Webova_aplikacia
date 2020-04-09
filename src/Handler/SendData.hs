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

getSendDataR :: Text -> DeviceId -> Text -> Handler Html
getSendDataR uuid id' value = do
    valid <- validation id' uuid
    if(valid)
        then do
            time <- liftIO myTime
            _ <- runDB $ insert $ Values value time id'
            defaultLayout [whamlet|<p>OK|]
        else defaultLayout [whamlet|<p>NIE|]



    

postSendDataR :: Text -> DeviceId -> Text -> Handler Html
postSendDataR uuid id' value = getSendDataR uuid id' value

validation :: DeviceId -> Text -> Handler Bool
validation id' uuid = do
  results <- runDB $ selectList [DeviceId ==. id', DeviceUuid ==. uuid] []
  case results of
    [] -> return False
    _  -> return True

myTime :: IO UTCTime
myTime = do
  utc <- getCurrentTime
  return $ addUTCTime (120*60) utc