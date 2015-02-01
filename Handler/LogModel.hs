{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : LogModel
Description : VMX Logging

This module contains functions for returning the last line of the
log.txt file inside the current session directory.
-}
module Handler.LogModel where

import Import
import System.Process
import Data.Text.IO (hGetContents)

{-|
OPTIONS for \/session\/#SessionId\/log.txt
-}
optionsLogModelR :: SessionId -> Handler ()
optionsLogModelR _ = do
    addHeader "Allow" "GET"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET"
    return ()

{-|
GET \/session\/#SessionId\/log.txt

Returns the last line of the log.txt file for the session
-}
getLogModelR :: SessionId -> Handler String
getLogModelR sid = do
   addHeader "Access-Control-Allow-Origin" "*"
   addHeader "Content-Type" "application/json"
   dataDir <- wwwDir
   let shellLine = unwords ["tail -1",dataDir++"sessions/"++sid++"/log.txt"]
   (_,Just stdoutHdl,_,hdl) <- lift $ createProcess (shell $ shellLine) {std_out = CreatePipe, close_fds = True}
   stdout <- liftIO $ Data.Text.IO.hGetContents stdoutHdl
   _ <- liftIO $ waitForProcess hdl
   return $ unpack stdout

