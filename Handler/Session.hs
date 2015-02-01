{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Session
Description : VMX Session management

Creating a listing sessions
-}
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

{-|
OPTIONS for \/session
-}
optionsSessionR :: Handler ()
optionsSessionR = do
    addHeader "Allow" "Get, Put"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET, PUT"
    return ()

{-|
Data structure for creating a new session
-}
data CreateSessionCommand = CreateSessionCommand {
        modelUUIDS :: [String]
} 

instance FromJSON CreateSessionCommand where
    parseJSON (Object o) = CreateSessionCommand <$> (o .: "uuids")
    parseJSON _ = mzero

{-|
POST \/session

Creates a new session
-}
postSessionR :: Handler String
postSessionR = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Content-Type" "application/json"
    (csc :: CreateSessionCommand ) <- requireJsonBody
    (_, payLoad) <- createSession (modelUUIDS csc)
    App {..} <- getYesod
    return payLoad

{-|
Creates a new session
-}
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
    (_,_,_,ph)      <- lift $ createProcess (shell $ shellLine)
                         {std_out = UseHandle log'}
    
    liftIO $ 
        waitForFile (sessionPath' ++ "/url") ph vmxExecutable'
    return $ (sid, asString $ object ["data" .= object ["session_id" .= sid]])
    where
        asString = C.unpack . C.concat . L.toChunks . encode
        waitForFile :: FilePath -> ProcessHandle -> String -> IO ()
        waitForFile f ph vmxExecutable' = do
            --liftIO $ print "Waiting..."
            ready <- doesFileExist f
            ec <- getProcessExitCode ph
            case ec of
              Nothing -> return () --liftIO $ print "good"
              Just e -> error $ "Error " ++ (show e) ++": Cannot Start " <> vmxExecutable'
            if ready
                then return ()
                else do
                    threadDelay 200
                    waitForFile f ph vmxExecutable'
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


{-|
GET /session
Lists session
-}
getSessionR :: Handler Value
getSessionR = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Content-Type" "application/json"
    list_sessions

{-|
Lists sessions
-}
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
{-|
Make a JSON

NOTE(TJM): what does this do?
-}
makeJson :: String -> Value
makeJson s = do
    -- String -> Char8 bystring
    let packed = C.pack s
    -- Char8 -> Lazy bytestring
    let chunked = L.fromChunks [packed]
    let eJ :: Either String Value = eitherDecode chunked
    case eJ of
        Right r -> r
        -- TODO .. properly handle errors
        Left _ -> undefined

