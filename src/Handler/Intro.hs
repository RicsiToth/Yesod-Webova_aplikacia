{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Intro where

import Import

getIntroR :: Handler Html
getIntroR = do
  maid <- maybeAuthId
  if(isJust maid)
    then redirect HomeR
  else do
    defaultLayout $ do
      setTitle "Welcome to Ročníkáč"
      $(widgetFile "intro")



