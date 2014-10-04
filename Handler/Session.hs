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
import Control.Monad (mzero)
import System.Posix.Files (namedPipeMode, createNamedPipe, accessModes, namedPipeMode)
import Data.Bits ((.|.))
import Data.UUID.V4 as U4 (nextRandom)
import Data.UUID as U (toString)
import System.Posix.Env(setEnv)
import Data.Aeson (decode, encode)

import Helper.Shared
import Control.Exception (tryJust)
import Control.Monad (guard, filterM)
import System.IO.Error (isDoesNotExistError)
import qualified Data.Text.IO as DT (readFile)
import Data.List (head,isInfixOf)

import Control.Concurrent.STM.TMVar
import Control.Monad.STM

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
    (sessionId, payLoad) <- createSession (modelUUIDS csc)
    App {..} <- getYesod
    return payLoad

type ModelName = String

-- no launching a new session became simpler, only using one pipe.. this function could be cleaned up a lot
createSession :: [String] -> Handler (SessionId, String)
createSession uuids = do
    App _ _ _ _ portMap' _  <- getYesod

    sid             <- liftIO getSessionId
    sessionPath'    <- sessionPath sid
    lift $    createDirectory sessionPath' 

    inputPipePath'  <- inputPipePath sid
    outputPipePath' <- outputPipePath sid
    outLogPath'     <- outLogPath sid
    vmxExecutable'  <- vmxExecutable

    port <- addLock portMap' sid

    dataDir <- wwwDir



    _  <- lift $ createNamedPipe inputPipePath'  (accessModes .|. namedPipeMode)

    let shellLine = unwords [vmxExecutable', dataDir, sid, name, ":" ++ show port]
    liftIO $ print "the line is"
    liftIO $ print shellLine
    log'    <- lift $ openFile outLogPath' AppendMode
    _       <- lift $ createProcess (shell $ shellLine)
                         {std_out = UseHandle log'} --, std_err = UseHandle log'}
    fromBrain      <- liftIO $ drainFifo outputPipePath'
    liftIO $ waitForFile (sessionPath' ++ "/url")
    return $ (sid, C.unpack $ C.concat $ L.toChunks $ encode $ object ["data" .= object ["session_id" .= sid]])
    where
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
        inputPipePath :: SessionId -> Handler String
        inputPipePath  sid = fmap (++ "/pipe") (sessionPath sid)>>= return
        outputPipePath  = inputPipePath
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


