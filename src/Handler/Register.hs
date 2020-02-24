{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Register where

import Import
import Yesod.Auth.HashDB (setPassword)

data NewUser = NewUser { nUsername :: Text,
                         nPassword :: Text
                       }

getRegisterR :: Handler Html
getRegisterR = do
  (widget, enctype) <- generateFormPost newUserForm
  defaultLayout $ do
    setTitle "Sign up"
    $(widgetFile "register")


postRegisterR :: Handler Html
postRegisterR = do
  ((result, widget), enctype) <- runFormPost newUserForm
  case result of
    FormSuccess nu -> do
      newb <- setPassword (nPassword nu) $
            User { userUsername = (nUsername nu), 
                   userPassword = ""
                 }

      nameTaken <- usernameTaken $ nUsername nu
      if (nameTaken)
        then do
          setMessage "That username is not available"
          defaultLayout $ do
            setTitle "Name Taken"
            $(widgetFile "register")
        else do
          _ <- runDB $ insert newb
          redirect $ AuthR LoginR
    _ -> do
      setMessage "Something went wrong"
      redirect RegisterR


usernameTaken :: Text -> Handler Bool
usernameTaken name = do
  results <- runDB $ selectList [UserUsername ==. name] []
  case results of
    [] -> return False
    _  -> return True


newUserForm :: Form NewUser
newUserForm = renderDivs $ NewUser
              <$> areq textField "Username" Nothing
              <*> areq passwordField "Password" Nothing