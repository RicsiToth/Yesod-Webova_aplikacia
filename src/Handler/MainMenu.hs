{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.MainMenu where

import Import

getMainMenuR :: Handler Html
getMainMenuR = defaultLayout $ do 
                    setTitle "McMafia - Online mafianska hra"
                    $(widgetFile "Game/Game")
