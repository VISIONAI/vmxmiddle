module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Default.Config
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Settings
import Settings.Development (development)
-- import qualified Database.Persist
import Settings (widgetFile, Extra (..), SessionId, ModelId)
import Model
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)
import Control.Concurrent.MVar
import Data.Map.Strict (Map)
import Data.IORef (IORef)
import System.Directory     (getCurrentDirectory,createDirectoryIfMissing,doesFileExist)
import Network.Wai as Wai
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.IORef (readIORef)
-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    -- , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    -- , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    , portMap      ::  MVar (Map String (MVar Int))
    , machineIdent :: IORef (Maybe String)
    , imageStream  :: IORef (Map String [String])
    , vmxVersion   :: IORef (String)
    }

instance HasHttpManager App where
    getHttpManager = httpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

approotViaRequest :: App -> Wai.Request -> T.Text
approotViaRequest ar req =
    case requestHeaderHost req of
        Just a  -> prefix `T.append` decodeUtf8 a
        Nothing -> appRoot $ settings ar
    where
        prefix =
            case isSecure req of
                True  -> "https://"
                False -> "http://"

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- uses the host flag in the request
    approot = ApprootRequest approotViaRequest

    -- if enabled use relative paths for redirects, so that
    -- a request for localhost:3000/sessions/ will give you /sessions
    --approot = ApprootRelative

    -- This will use the appRoot inside the config file, so that a
    -- request for http://retbone.local:3000/sessions/ will give you
    -- http://localhost:3000/sessions assuming that localhost:300 was
    -- your approot in the config file
    --approot = ApprootMaster $ appRoot . settings

    --e <- liftIO $ tryJust (guard . isDoesNotExistError) (readFile "version")
    --let versionMiddle = either (const "development") id e


    yesodMiddleware handler = do
      addHeader "Access-Control-Allow-Origin" "*"
      addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type,Origin, X-Requested-With, Accept"
      App _ _ _ _ _ _ _ vmxVersion' <- getYesod
      v <- liftIO $ readIORef vmxVersion'
      setHeader "Server" $ T.pack v
      
      --addHeader "Access-Control-Allow-Methods" "GET, PUT, POST, DELETE, OPTIONS"
      --addHeader "Access-Control-Allow-Headers" "Accept, Origin, X-Requested-With"
      handler


    errorHandler (InternalError e) = 
        selectRep $ do
            provideRepType  "application/json" $ return $ object ["error" .= e] 
            provideRep $ defaultLayout $ 
                toWidget [hamlet|<h1>500 error</h1><p> #{e}|]

    errorHandler (InvalidArgs e) = 
        selectRep $ do
            provideRepType  "application/json" $ return $ object ["error" .= e] 
            provideRep $ defaultLayout $ 
                toWidget [hamlet|<h1>invalid args</h1><p> #{show e}|]

    errorHandler NotFound = 
        selectRep $ do
            provideRepType  "application/json" $ return $ object ["error" .= ("Not Found" :: String)] 
            provideRep $ defaultLayout $ 
                toWidget [hamlet|<h1>Not Found</h1><p> This resource cannot be found.|]

    errorHandler other =
      selectRep $ do
            provideRepType  "application/json" $ return $ object ["error" .= ("Other Error: "++ (show other))] 
            provideRep $ defaultLayout $ 
                toWidget [hamlet|<h1>other error</h1><p> #{show other}|]
      --defaultErrorHandler other

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do

            $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing


    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

    -- allow a maximum upload of 10 MB
    maximumContentLength _ (Just (ModelR {})) = Just (10 * 1024 * 1024)
    maximumContentLength _ _ = Just (2 * 1024 * 1024)
-- How to run database actions.
-- instance YesodPersist App where
--     type YesodPersistBackend App = SqlPersistT
--     runDB = defaultRunDB persistConfig connPool
-- instance YesodPersistRunner App where
--     getDBRunner = defaultGetDBRunner connPool

-- instance YesodAuth App where
--     type AuthId App = UserId
-- 
--     -- Where to send a user after successful login
--     loginDest _ = HomeR
--     -- Where to send a user after logout
--     logoutDest _ = HomeR
-- 
--     getAuthId creds = runDB $ do
--         x <- getBy $ UniqueUser $ credsIdent creds
--         case x of
--             Just (Entity uid _) -> return $ Just uid
--             Nothing -> do
--                 fmap Just $ insert User
--                     { userIdent = credsIdent creds
--                     , userPassword = Nothing
--                     }
-- 
--     -- You can add other plugins like BrowserID, email or OAuth here
--     authPlugins _ = [authBrowserId def, authGoogleEmail]
-- 
--     authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

getVmxPrefix :: Char -> String -> String
getVmxPrefix '/' _ = ""
getVmxPrefix _ cwd = cwd ++ "/"

-- Does nothing for absolute locations (Starting with /) but will
-- prepend the current work directory if the specief path is not
-- relative (not starging with a /)
finalPath :: String -> String -> String
finalPath cwd s = do
  let firstChar = head s
  let prefix = getVmxPrefix firstChar cwd
  prefix ++ s
  
wwwDir :: Handler String
wwwDir = do
    extra <- getExtra
    cwd <- liftIO $ getCurrentDirectory
    case extraWwwDir extra of
        Just theDir -> do
          let result = finalPath cwd $ theDir ++ "/"
          let session_dir = result ++ "sessions"
          let models_dir = result ++ "models"
          liftIO $ createDirectoryIfMissing True session_dir
          liftIO $ createDirectoryIfMissing False models_dir
          return result
        Nothing  -> return "/www/vmx/"

vmxExecutable :: Handler String
vmxExecutable = do
    extra <- getExtra
    cwd <- liftIO $ getCurrentDirectory
    case extraVmxPath extra of
        Just theDir -> do
          let result = finalPath cwd $ theDir ++ "/VMXserver"
          exist <- liftIO $ doesFileExist result
          _ <- case exist of
            True -> return () -- liftIO $ print $ result ++ " exists"
            False -> do
              liftIO $ print $ "Warning " ++ result ++ " does not exist"
              return ()
          return result
        Nothing  -> return "/home/g/VMXserver/VMXserver"

