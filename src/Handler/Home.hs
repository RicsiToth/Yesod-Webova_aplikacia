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
module Handler.Home where

import Import
import Text.Julius (RawJS (..))
import Database.Persist.Sql

getHomeR :: Handler Html
getHomeR = do
  let id'= toSqlKey 1
  runDB $ insert $ Device "Prvy device" id'
  defaultLayout $ do
    setTitle "Welcome to McMafia"
    $(widgetFile "homepage")
    
widget = do 
  [whamlet|
    <p> 
      <a href=@{AuthR LoginR} .login> Prihlásenie
      <a href=@{RegisterR} .signin> Regstrácia 
  |]
  toWidget [lucius| 
              .login:link, .login:visited{
                  background-color: black;
                  color: white;
                  padding: 14px 25px;
                  text-align: center;
                  text-decoration: none;
                  display: inline-block;
                  font-size: 25px;
                  border-radius: 10px;
                  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.5), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
                  position: relative;
                  left: 60px;
                  top: 50px;
              }
              
              .login:hover, .login:active{
                  box-shadow: 0 4px 8px 0 rgba(100, 0, 0, 0.5), 0 6px 20px 0 rgba(100, 0, 0, 0.19);
                  color: red;
              }

              .signin:link, .signin:visited{
                  background-color: black;
                  color: white;
                  padding: 14px 25px;
                  text-align: center;
                  text-decoration: none;
                  display: inline-block;
                  font-size: 25px;
                  border-radius: 10px;
                  box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.5), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
                  position: relative;
                  left: 90px;
                  top: 50px;
              }
              
              .signin:hover, .signin:active{
                  box-shadow: 0 4px 8px 0 rgba(100, 0, 0, 0.5), 0 6px 20px 0 rgba(100, 0, 0, 0.19);
                  color: red;
              }
            |]



