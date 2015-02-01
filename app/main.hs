import Import
import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMainLog)
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
  putStrLn "Welcome to VMXMiddle (c) 2013-2015 vision.ai"
  putStrLn "Please visit http://localhost:3000 in your browser"
  do
    cwd <- getCurrentDirectory
    defaultMainLog (withArgs ["Production"] (fromArgs parseExtra)) makeApplication

