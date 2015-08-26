{-# LANGUAGE ScopedTypeVariables #-}
module Handler.LogModel where

import Import


import System.Process
import Data.Text.IO (hGetContents)


getLogModelR :: SessionId -> Handler String
getLogModelR sid = do
   addHeader "Content-Type" "application/json"

   dataDir         <- wwwDir
   let shellLine = unwords ["tail -1",dataDir++"sessions/"++sid++"/log.txt"]
   (_,Just stdoutHdl,_,hdl)      <- lift $ createProcess (shell $ shellLine) {std_out = CreatePipe, close_fds = True}
   stdout <- liftIO $ Data.Text.IO.hGetContents stdoutHdl
   exitCode <- liftIO $ waitForProcess hdl
   return $ unpack stdout

   --let shellLine = ["-1",dataDir++"sessions/"++sid++"/log.txt"]
   --stdout <- liftIO $ readProcess "tail" shellLine []
   --return $ stdout

optionsLogModelR :: SessionId -> Handler ()
optionsLogModelR _ = do
    addHeader "Allow" "POST"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET"
    return ()
