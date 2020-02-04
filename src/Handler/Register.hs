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
                         nFullName :: Text,
                         nPassword :: Text,
                         nEmail :: Text
                       }

getRegisterR :: Handler Html
getRegisterR = do
  (tmp, enctype) <- generateFormPost newUserForm
  let widget = myWid tmp enctype
  defaultLayout $ do
    setTitle "Sign up"
    $(widgetFile "homepage")


postRegisterR :: Handler Html
postRegisterR = do
  ((result, tmp), enctype) <- runFormPost newUserForm
  let widget = myWid tmp enctype
  case result of
    FormSuccess nu -> do
      newb <- setPassword (nPassword nu) $
            User { userUsername = (nUsername nu), 
                   userPassword = "",
                   userFullName = (nFullName nu),
                   userAdmin = False,
                   userEmail = (nEmail nu)
                 }

      nameTaken <- usernameTaken $ nUsername nu
      -- check username availability before insert
      -- avoid internal server error when using Mongo
      -- with a unique index on User collection
      if (nameTaken)
        then do
          setMessage "That username is not available"
          defaultLayout $ do -- redisplay the page, keeping form data
            setTitle "Name Taken"
            $(widgetFile "homepage")
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
              <*> areq textField "Full Name" Nothing
              <*> areq passwordField "Password" Nothing
              <*> areq textField "Email" Nothing

myWid widget enctype = do 
  [whamlet|
    <form method="post" enctype=#{enctype}>
      ^{widget}
      <button> Poslať
  |]
  toWidget [lucius| 
             form{
               position: relative;
               left: 120px;
             }
             #hident5{
               position: relative;
               left: 42px;
             }
             #hident4{
               position: relative;
               left: 14.5px;
             }
             #hident3{
               position: relative;
               left: 12px;
             }
             #hident2{
               position: relative;
               left: 11px;
             }
            |]