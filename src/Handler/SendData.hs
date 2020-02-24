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

getSendDataR :: DeviceId -> Text -> Handler Html
getSendDataR id' value = do
    _ <- runDB $ insert $ Values value id'
    defaultLayout [whamlet|<p>OK|]

    

postSendDataR :: DeviceId -> Text -> Handler Html
postSendDataR id' value = getSendDataR id' value
