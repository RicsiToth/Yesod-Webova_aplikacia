{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Control.Monad.Logger (LogSource)
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Network.HTTP.Conduit (Manager)
import Data.Time.Clock

import Yesod.Auth.HashDB

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

type DB a = forall (m :: * -> *).
    (MonadIO m) => ReaderT SqlBackend m a

instance Yesod App where
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        maid <- maybeAuthId
        _ <- getYesod
        _ <- getMessage   
        _ <- getCurrentRoute
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    authRoute :: App -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized :: Route App -> Bool -> Handler AuthResult
    isAuthorized IntroR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized RegisterR _ = return Authorized
    isAuthorized (SendDataR _ _ _) _ = return Authorized
    isAuthorized (SendDataTimeR _ _ _ _) _ = return Authorized
    isAuthorized ProfileR _ = isAuthenticated
    isAuthorized AddDeviceR _ = isAuthenticated
    isAuthorized (RemoveDeviceR _) _ = isAuthenticated
    isAuthorized (HomeIdR _) _ = isAuthenticated
    isAuthorized HomeR _ = isAuthenticated
    isAuthorized (DataR _) _ = return Authorized
    isAuthorized (ChangeGraphR _ _) _ = isAuthenticated

    addStaticContent :: Text -> Text -> LByteString -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        genFileName lbs = "autogen-" ++ base64md5 lbs

    
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger


instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    loginDest _ = HomeR

    logoutDest _ = IntroR

    authPlugins _ = [authHashDB (Just . UniqueUsername)]
  
    authenticate :: (MonadHandler m, HandlerSite m ~ App) => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUsername $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> Authenticated <$> insert User
                { userUsername = credsIdent creds
                , userPassword = ""
                }

isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized


instance YesodAuthPersist App

instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

