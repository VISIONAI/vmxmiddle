import Import
import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMainLog)
import Settings             (parseExtra)
import Application          (makeApplication)

import System.Environment   (getEnv, withArgs, getExecutablePath)
import Control.Monad

import System.Directory     (setCurrentDirectory,getCurrentDirectory)

import System.FilePath.Posix (takeDirectory)
import Control.Monad.Trans (liftIO)


main :: IO ()
main = do
  exec_path <- getExecutablePath
  setCurrentDirectory ( takeDirectory exec_path)
  do
    cwd <- getCurrentDirectory
    liftIO (print "Welcome to VMXMiddle v1.0 (c) 2013-2014 vision.ai, LLC")
     --liftIO ( print ("cwd is " ++ cwd))
    defaultMainLog (withArgs ["Production"] (fromArgs parseExtra)) makeApplication

