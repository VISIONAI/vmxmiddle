{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Session where

import Import
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import System.IO 
import System.Process
import System.Directory (getDirectoryContents, createDirectory, doesFileExist)
import Data.UUID.V4 as U4 (nextRandom)
import Data.UUID as U (toString)
import Data.Aeson (encode)

import Helper.Shared
import Control.Exception (tryJust)
import Control.Monad (guard)
import System.IO.Error (isDoesNotExistError)
import qualified Data.Text.IO as DT (readFile)
import Data.List (isInfixOf)


import Control.Concurrent (threadDelay)

optionsSessionR :: Handler ()
optionsSessionR = do
    addHeader "Allow" "Get, Put"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET, PUT"
    return ()




postSessionR :: Handler String
postSessionR = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Content-Type" "application/json"
    (csc :: CreateSessionCommand ) <- requireJsonBody
    (_, payLoad) <- createSession (modelUUIDS csc)
    App {..} <- getYesod
    return payLoad

type ModelName = String

-- no launching a new session became simpler, only using one pipe.. this function could be cleaned up a lot
createSession :: [String] -> Handler (SessionId, String)
createSession uuids = do
    sid             <- liftIO getSessionId
    sessionPath'    <- sessionPath sid
    lift $ 
        createDirectory sessionPath' 

    outLogPath'     <- outLogPath sid
    vmxExecutable'  <- vmxExecutable
    port            <- addLock sid Nothing
    dataDir         <- wwwDir

    let shellLine = unwords [vmxExecutable', dataDir, sid, name, ":" ++ show port]

    log'    <- lift $ openFile outLogPath' AppendMode
    _       <- lift $ createProcess (shell $ shellLine)
                         {std_out = UseHandle log'} --, std_err = UseHandle log'}
    liftIO $ 
        waitForFile (sessionPath' ++ "/url")
    return $ (sid, asString $ object ["data" .= object ["session_id" .= sid]])
    where
        asString = C.unpack . C.concat . L.toChunks . encode
        waitForFile :: FilePath -> IO ()
        waitForFile f = do
            ready <- doesFileExist f
            if ready
                then return ()
                else do
                    threadDelay 200
                    waitForFile f
        name = case length uuids of
                0 -> "none"
                _ -> uuids !! 0
        getSessionId :: IO String
        getSessionId = do
            seed <- U4.nextRandom
            return $ U.toString seed
        sessionPath :: SessionId -> Handler String
        sessionPath  sid = do
            dir <- wwwDir 
            return $ dir ++ "sessions/" ++ sid
        outLogPath :: SessionId -> Handler String
        outLogPath  sid = fmap (++ "/log.txt") (sessionPath sid) >>= return
        --modelPath = case modelNameM of
        --        Just m -> "models/" ++ m ++ ".mat"
        --        Nothing -> ""


--list all sessions
getSessionR :: Handler Value
getSessionR = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Content-Type" "application/json"
    list_sessions

list_sessions :: Handler Value
list_sessions = do
    sessions <-  sp >>= lift.getDirectoryContents 
    let sessions'' = filter notDots sessions
    psOutput <- liftIO $ readProcess "ps" ["aux"] ""
    let sessions' = filter (notDead psOutput) sessions''
    out <- sequence $ map getSessionInfo sessions'
    return $ object ["data" .= out]
    where
        sp = fmap (++ "sessions/") wwwDir 
        getSessionInfo :: FilePath -> Handler Value
        getSessionInfo fp = do
            sp' <- sp
            mModelJson <- liftIO $ tryJust (guard . isDoesNotExistError) (DT.readFile  $ sp' ++ fp ++ "/model.json")
            case mModelJson of
                Right modelJson -> 
                    return $ object ["session" .= fp, "model" .= (makeJson . unpack)  modelJson]
                Left _ -> return $ object ["session" .= fp, "error" .= True]
        notDead :: String -> FilePath -> Bool
        notDead psOutput sessionDir = do
            let possible = filter (isInfixOf sessionDir) (lines psOutput)
            (not . null $ filter (isInfixOf "VMXserver") possible)
        --check if running
        notDots :: FilePath -> Bool
        notDots fp = case fp of
                        "."         -> False
                        ".."        -> False
                        ".DS_Store" -> False
                        _ -> True

-- create a new session
data VmxSessionFile = VmxSessionFile {
        pid :: String
} 



instance FromJSON VmxSessionFile where
    parseJSON (Object o) = VmxSessionFile <$> (o .: "pid")
    parseJSON _ = mzero


-- create a new session
data CreateSessionCommand = CreateSessionCommand {
        modelUUIDS :: [String]
} 



instance FromJSON CreateSessionCommand where
    parseJSON (Object o) = CreateSessionCommand <$> (o .: "uuids")
    parseJSON _ = mzero


