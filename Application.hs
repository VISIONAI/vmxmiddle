{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
-- import qualified Database.Persist
-- import Database.Persist.Sql (runMigration)
import Network.HTTP.Client.Conduit (newManager)
import Control.Concurrent (forkIO, threadDelay)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize, flushLogStr)
import Network.Wai.Logger (clockDateCacher)
import Data.Default (def)
import Yesod.Core.Types (loggerSet, Logger (Logger))

-- Imports for map of SessionIds to MVars
import qualified Data.Map.Strict as Map
import Data.IORef (newIORef)
import Control.Exception (tryJust)
import System.IO.Error (isDoesNotExistError)
import Control.Monad (guard)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Session
import Handler.ManageSession
import Handler.ProcessImage
import Handler.SessionParams
import Handler.SessionConfig
import Handler.EditModel
import Handler.CreateModel
import Handler.Model
import Handler.ModelImage
import Handler.ModelData
import Handler.ModelViewer
import Handler.SessionViewer
import Handler.RandomImage
import Handler.CheckLicense
import Handler.ActivateLicense
import Handler.LoadModel
import Handler.LogModel
import Handler.StreamImages
import Handler.Forward
import Handler.SaveSession


-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ defaultMiddlewaresNoLogging app, logFunc)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager
    s <- staticSite
--    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
--              Database.Persist.loadConfig >>=
--              Database.Persist.applyEnv
--    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, updater) <- clockDateCacher

    -- If the Yesod logger (as opposed to the request logger middleware) is
    -- used less than once a second on average, you may prefer to omit this
    -- thread and use "(updater >> getter)" in place of "getter" below.  That
    -- would update the cache every time it is used, instead of every second.
    let updateLoop = do
            threadDelay 1000000
            updater
            flushLogStr loggerSet'
            updateLoop
    _ <- forkIO updateLoop

    -- Create Map of UUID -> Semaphores to control atomic writes to pipe
    portMapMVar <- liftIO $ newMVar $ Map.fromList []

    e <- liftIO $ tryJust (guard . isDoesNotExistError) (readFile "version")
    let versionMiddle = either (const "development") id e


    vmxVersionVar <- liftIO $ newIORef versionMiddle -- $ either (const "development") id $ tryJust (guard . isDoesNotExistError) (readFile "version")
    --vmxVersionVar <- liftIO $ newIORef ("monkey"::String) --either (const "development") id $ tryJust (guard . isDoesNotExistError) (readFile "version")

    -- Keep track of the machineIdent string we get back from vmxserver
    machineIdentIORef <- liftIO $ newIORef Nothing
    modelImageCache   <- liftIO $ newIORef $ Map.fromList []



    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf s manager logger portMapMVar machineIdentIORef modelImageCache vmxVersionVar

    -- Perform database migration using our application's logging settings.
--     runLoggingT
--         (Database.Persist.runPool dbconf (runMigration migrateAll) p)
--         (messageLoggerSource foundation logger)
-- 
    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }


