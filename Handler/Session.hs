{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Session where

import Import
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import System.IO 
import System.Process
import System.Directory (getDirectoryContents, createDirectory)
import Control.Monad (mzero)
import System.Posix.Files (namedPipeMode, createNamedPipe, accessModes, namedPipeMode)
import Data.Bits ((.|.))
import Data.UUID.V4 as U4 (nextRandom)
import Data.UUID as U (toString)
import System.Posix.Env(setEnv)

import Helper.Shared
import Control.Exception (tryJust)
import Control.Monad (guard)
import System.IO.Error (isDoesNotExistError)

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
    sessionId <- createSession (modelUUIDS csc)
    return sessionId

type ModelName = String

-- no launching a new session became simpler, only using one pipe.. this function could be cleaned up a lot
createSession :: [String] -> Handler String
createSession uuids = do
    let name = case length uuids of
                    0 -> "none"
                    _ -> uuids !! 0
    liftIO $ print name
    sid   <- liftIO getSessionId
    lift $ setEnv "MCR_CACHE_ROOT" "/tmp/mcr_cache" False
    sessionPath' <- sessionPath sid
    lift $ createDirectory sessionPath' 
    inputPipePath' <- inputPipePath sid
    outputPipePath' <- outputPipePath sid
    outLogPath' <- outLogPath sid
    vmxExecutable' <- vmxExecutable
    matlabRuntime' <- matlabPath
    wwwDir' <- wwwDir
    _  <- lift $ createNamedPipe inputPipePath'  (accessModes .|. namedPipeMode)
    log'        <- lift $ openFile outLogPath' AppendMode
    _          <- lift $ createProcess (shell $ unwords [vmxExecutable', matlabRuntime', wwwDir', sid, name])
                         {std_out = UseHandle log', std_err = UseHandle log'}
    fromBrain      <- liftIO $ drainFifo outputPipePath'
    return fromBrain
    where
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
        outputPipePath :: SessionId -> Handler String
        outputPipePath  sid = fmap (++ "/pipe") (sessionPath sid) >>= return
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
    let sessions' = filter notDots sessions

    out <- sequence $ map getSessionInfo sessions'
    return $ object ["data" .= out]
    where
        sp = fmap (++ "sessions/") wwwDir 
        getSessionInfo :: FilePath -> Handler Value
        getSessionInfo fp = do
            sp' <- sp
            mModelJson <- liftIO $ tryJust (guard . isDoesNotExistError) (readFile  $ sp' ++ fp ++ "/model.json")
            case mModelJson of
                Right modelJson -> 
                    return $ object ["session" .= fp, "model" .= makeJson modelJson]
                Left _ -> return $ object ["session" .= fp, "error" .= True]
        notDots :: FilePath -> Bool
        notDots fp = case fp of
                        "." -> False
                        ".." -> False
                        ".DS_Store" -> False
                        _ -> True


-- create a new session
data CreateSessionCommand = CreateSessionCommand {
        modelUUIDS :: [String]
} 



instance FromJSON CreateSessionCommand where
    parseJSON (Object o) = CreateSessionCommand <$> (o .: "uuids")
    parseJSON _ = mzero


