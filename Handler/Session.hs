{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Session where

import Import
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import System.IO 
import System.Process
import System.Directory (getDirectoryContents, createDirectory)
import Data.Aeson (eitherDecode, (.:?))
import Control.Monad (mzero)
import System.Posix.Files (namedPipeMode, createNamedPipe, accessModes, namedPipeMode)
import Data.Bits ((.|.))
import Data.UUID.V4 as U4 (nextRandom)
import Data.UUID as U (toString)
import System.Posix.Env(setEnv)

import Helper.Shared





postSessionR :: Handler String
postSessionR = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Content-Type" "application/json"
    (csc :: CreateSessionCommand ) <- parseJsonBody_
    sessionId      <- liftIO getSessionId
    outputPipePath <- createSession sessionId csc
    fromBrain      <- liftIO $ drainFifo outputPipePath
    return fromBrain
    where
        getSessionId :: IO String
        getSessionId = do
            seed <- U4.nextRandom
            return $ U.toString seed
        -- this probably should receive a modelName directly rather than a CreateSessionCommand
        createSession :: String -> CreateSessionCommand -> Handler FilePath
        createSession sid csc = do
            lift $ setEnv "MCR_CACHE_ROOT" "/tmp/mcr_cache" False
            sessionPath' <- sessionPath
            lift $ createDirectory sessionPath'
            inputPipePath' <- inputPipePath
            outputPipePath' <- outputPipePath
            outLogPath' <- outLogPath
            inputPipe  <- lift $ createNamedPipe inputPipePath'  (accessModes .|. namedPipeMode)
            outputPipe <- lift $ createNamedPipe outputPipePath' (accessModes .|. namedPipeMode)
            log        <- lift $ openFile outLogPath' AppendMode
            _          <- lift $ createProcess (shell $ unwords [vmxExecutable, matlabRuntime, sessionPath', inputPipePath', outputPipePath', modelPath])
                                 {std_out = UseHandle log, std_err = UseHandle log}
            return outputPipePath'
            where
                sessionPath :: Handler String
                sessionPath  = do
                    dir <- wwwDir 
                    return $ dir ++ "sessions/" ++ sid
                inputPipePath :: Handler String
                inputPipePath  = fmap (++ "/pipe_input") sessionPath >>= return
                outputPipePath :: Handler String
                outputPipePath  = fmap (++ "/pipe_output") sessionPath >>= return
                outLogPath :: Handler String
                outLogPath  = fmap (++ "/log.txt") sessionPath >>= return
                vmxExecutable = "/home/g/build/run_VMXserver.sh"
                matlabRuntime = "/home/g/build/MATLAB/R2013a"
                modelPath = case csc of
                                CreateSessionCommand (Just m) ->  "models/" ++ m ++ ".mat"
                                CreateSessionCommand Nothing  -> ""


--list all sessions
getSessionR :: Handler Value
getSessionR = do
    addHeader "Access-Control-Allow-Origin" "*"
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
            modelJson <- lift $ readFile (sp' ++ fp ++ "/model.json")
            return $ object ["session" .= fp, "model" .= makeJson modelJson]
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
                Left e -> undefined
        notDots :: FilePath -> Bool
        notDots fp = case fp of
                        "." -> False
                        ".." -> False
                        ".DS_Store" -> False
                        _ -> True


-- create a new session
data CreateSessionCommand = CreateSessionCommand {
        modelName :: Maybe String
} 



instance FromJSON CreateSessionCommand where
    parseJSON (Object o) = CreateSessionCommand <$> (o .:? "model_name")
    parseJSON _ = mzero


