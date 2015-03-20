import Import
import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Settings             (parseExtra)
import Application          (makeApplication)

import System.Environment   (getEnv, withArgs, getExecutablePath)
--import Control.Monad

import System.Directory     (setCurrentDirectory,getCurrentDirectory)

import System.FilePath.Posix (takeDirectory)
--import Control.Monad.Trans (liftIO)


main :: IO ()
main = do
  exec_path <- getExecutablePath
  setCurrentDirectory ( takeDirectory exec_path)
  putStrLn "Welcome to VMXMiddle (c) 2013-2015 vision.ai, LLC"
  putStrLn "Please visit http://localhost:3000 in your browser"
  do
    cwd <- getCurrentDirectory
    defaultMainLog (withArgs ["Production"] (fromArgs parseExtra)) makeApplication


defaultMainLog :: (Show env, Read env)
               => IO (AppConfig env extra)
               -> (AppConfig env extra -> IO (Application, LogFunc))
               -> IO ()
defaultMainLog load getApp = do
    config <- load
    (app, logFunc) <- getApp config
    runSettings defaultSettings
        { settingsPort = appPort config
        , settingsHost = appHost config
        , settingsTimeout = 90
        , settingsOnException = const $ \e -> logFunc
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e)
        } app

