{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Helper.Shared
    ( drainFifo
    , headers
    , getPipeResponse
    , InputPipe
    , OutputPipe
    , makeJson
    , processImage
    , waitLock
    , releaseLock
    , exitVMXServer
    , delVMXFolder
    , addLock
    ) where

import Import
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import System.Directory (getDirectoryContents, createDirectory, removeDirectoryRecursive)
import System.Process
import System.IO
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson (encode,decode)
import GHC.IO.Handle.FD (openFileBlocking)
import Yesod.WebSockets
import Network.HTTP.Types (status400)
import Control.Exception (evaluate)

import Data.Map.Strict as SM (member, (!), insert, toList, Map (..)) 
import Data.IORef (atomicModifyIORef', readIORef)

import Control.Exception (try)
import System.IO.Error
import Helper.VMXTypes


import Control.Concurrent.STM.TMVar
import Control.Monad.STM
import Debug.Trace
import System.IO.Unsafe ( unsafePerformIO )
import GHC.Conc.Sync (unsafeIOToSTM)

type LockMap = SM.Map String (TMVar ())

releaseLock :: SessionId -> LockMap -> STM ()
releaseLock sid locks = putTMVar (locks ! sid) ()

addLock :: TMVar LockMap -> SessionId -> TMVar () -> STM ()
addLock lm sid newLock = do
    lockMap <- takeTMVar  lm
    let newMap = if (member sid lockMap)
                        then error "trying to add a lock that already exists"
                        else SM.insert sid newLock lockMap
    putTMVar lm newMap
    return ()

waitLock :: SessionId -> LockMap -> STM ()
waitLock sid locks = do
    if member sid locks
        then takeTMVar (locks ! sid)
        else retry

-- we use drainFifo instead of a normal readFile because Haskell's non-blocking IO treats FIFOs wrong
drainFifo :: FilePath -> IO String
drainFifo f = do
    hdl <- trace ("trying to open " ++ f ++ "in drainDifo") $ openFileBlocking f ReadMode
    t <- hIsEOF hdl
    if t then do
            trace ("fifo not ready, dropping handler") $ hClose hdl
            trace ("fifo not ready, dropping calling drainfifo again") $ drainFifo f
         else do
            o <- trace ("getting contents from " ++ f) $ hGetContents hdl
            _ <- trace ("forcing strict IO on " ++ f)  $ evaluate (length o)
            trace ("closing the handler after successful drain on " ++ f) $ hClose hdl
            return o

headers :: Handler ()
headers = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Content-Type" "application/json"

type InputPipe = FilePath
type OutputPipe = FilePath

getPipeResponse :: Value -> SessionId -> Handler String
getPipeResponse v sid = do
    App _ _ _ _ lm  _  <- getYesod
    locks' <- liftIO . atomically $ takeTMVar lm

    trace ("GPR: preprocessing LockMap for valid sid=" ++ sid) $ liftIO $ print "null"
    -- make sure we have a lock for this one
    liftIO $ if member sid locks'
        then do
            atomically $ putTMVar lm locks'
        else do
            newLock <- newTMVarIO ()
            atomically $ do
                putTMVar lm locks'
                addLock lm sid newLock

    locks <- liftIO . atomically $ takeTMVar lm

    trace ("GPR: taking the lock for " ++ sid) $ liftIO $ print "null"
    liftIO . atomically $ waitLock sid locks
    trace ("GPR: putting the LockMap back for " ++ sid) $ liftIO $ print "null"
    liftIO . atomically $ putTMVar lm locks
    i    <- getInputPipe  sid
    o    <- getOutputPipe sid
    fileE <-  trace ("GPR: trying to open the file for writing with " ++ sid) $ lift $ try (openFileBlocking i WriteMode)
    case fileE of
                -- An IOError here means the process that was supposed
                -- to read from the pipe died before it could, and matlab
                -- won't read again until we eat its (now useless) output
                Left (e :: IOError)->   do
                    _ <- lift $ drainFifo o
                    liftIO . atomically $ releaseLock sid locks
                    error $ show e
                    --lift $ openFileBlocking i WriteMode >>= return
                Right fileHandle -> do
                    trace ("GPR: writing payload to pipe with " ++ sid) $ lift $ L.hPutStr fileHandle payload
                    trace ("GPR: closing pipe with " ++ sid) $ lift $ hClose fileHandle
                    ret <- trace ("GPR: draining fifo for " ++ sid) $ lift $ drainFifo o
                    trace ("GPR: releasing lock for " ++ sid) $ liftIO $ print "null"
                    liftIO . atomically $ releaseLock sid locks
                    return ret
    where
        payload = encode v

getInputPipe  sid = fmap (++ "sessions/" ++ sid ++ "/pipe")  wwwDir 
getOutputPipe sid = fmap (++ "sessions/" ++ sid ++ "/pipe")  wwwDir 
lockFilePath sid =  fmap (++ "sessions/" ++ sid ++ "/modelupdate.lock") wwwDir

data HTTPVerb = 
    GET   |
    POST  |
    PUT   |
    DELETE

data ResourceV = 
    Model          |
    EditModel      |
    Session        |
    ProcessImage   |
    SessionParams
    
    

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
                Left _ -> undefined
        notDots :: FilePath -> Bool
        notDots fp = case fp of
                        "." -> False
                        ".." -> False
                        ".DS_Store" -> False
                        _ -> True




processImage :: SessionId -> [VMXImage] -> VMXParams -> String -> Handler String
processImage sid image params name = do
   let req = object ["command" .= command, "name" .= name, "images" .= image, "params" .= params]
   response <- getPipeResponse req sid
   return response
   where
        command :: String
        command = "process_image"

exitVMXServer :: SessionId -> Handler String
exitVMXServer sid = getPipeResponse (object ["command" .= exit]) sid >>=  return
	where 	
		exit :: String
		exit = "exit"

delVMXFolder :: FilePath -> Handler ()
delVMXFolder fp = do
	wwwDir' <- wwwDir
	liftIO $ removeDirectoryRecursive $ wwwDir' <> fp >>= return
    

    
instance WebSocketsData Value where
   toLazyByteString v = encode v
   fromLazyByteString s = do
        case decode s of
            Just val -> val
            Nothing -> object []





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

    

