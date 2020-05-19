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

getDataR :: DeviceId -> Handler Value
getDataR id' = do
    tmp <- runDB $ selectList [ValuesDeviceId ==. id'] [Asc ValuesTime]
    returnJson (map entityVal tmp)