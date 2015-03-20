{-# LANGUAGE PackageImports #-}
import "middle" Application (getApplicationDev)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, setPort, setTimeout)
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    forkIO $ runSettings (setTimeout 90 $ setPort port $ defaultSettings) app
    loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "yesod-devel/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
