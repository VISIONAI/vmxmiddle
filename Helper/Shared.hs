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
    , releasePort
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

import Data.Text.IO (hGetContents)

import Control.Concurrent.STM.TMVar
import Control.Monad.STM
import Debug.Trace
import System.IO.Unsafe ( unsafePerformIO )
import GHC.Conc.Sync (unsafeIOToSTM)

import Data.Maybe (fromJust)

type LockMap = SM.Map String (MVar Int)

type Port = Int

releasePort :: SessionId -> LockMap -> Port -> IO ()
releasePort sid locks port = putMVar (locks ! sid) port >> return ()

addLock :: MVar LockMap -> SessionId -> Handler LockMap
addLock portMap' sid = do
    portMap <- liftIO $ takeMVar portMap'
    newMap <- if (member sid portMap)
                        then error "trying to add a lock that already exists"
                        else do
                            port <- nextPort
                            newLock <- liftIO $ newMVar port
                            return $ SM.insert sid newLock portMap
    liftIO $ putMVar portMap' newMap
    return portMap

nextPort :: Handler Int
nextPort = do
    App _ _ _ _ _ lastPort'  _  <- getYesod
    np <- liftIO $ (+1) <$> takeMVar lastPort'
    liftIO $ putMVar lastPort' np
    return np

waitLock :: SessionId -> LockMap -> IO Int
waitLock sid locks = takeMVar (locks ! sid) >>= return

-- we use drainFifo instead of a normal readFile because Haskell's non-blocking IO treats FIFOs wrong
drainFifo :: FilePath -> IO String
drainFifo f = do
    hdl <- openFileBlocking f ReadMode
    o   <- Data.Text.IO.hGetContents hdl
    return $ unpack o

headers :: Handler ()
headers = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Content-Type" "application/json"

type InputPipe = FilePath
type OutputPipe = FilePath

getPipeResponse :: Value -> SessionId -> Handler String
getPipeResponse v sid = do
    App _ _ _ _ lm _ _  <- getYesod
    locks' <- liftIO $ takeMVar lm

    -- make sure we have a lock for this one
    locks <- if member sid locks'
                    then return locks'
                    else addLock lm sid

    locks <- liftIO $ takeMVar lm

    port <- liftIO  $ waitLock sid locks
    liftIO  $ putMVar lm locks
    i    <- getInputPipe  sid
    o    <- getOutputPipe sid
    fileE <-  lift $ try (openFileBlocking i WriteMode)
    case fileE of
                -- An IOError here means the process that was supposed
                -- to read from the pipe died before it could, and matlab
                -- won't read again until we eat its (now useless) output
                Left (e :: IOError)->   do
                    _ <- lift $ drainFifo o
                    liftIO  $ releasePort sid locks port
                    error $ show e
                    --lift $ openFileBlocking i WriteMode >>= return
                Right fileHandle -> do
                    liftIO $ L.hPutStr fileHandle payload
                    lift $ hClose fileHandle
                    ret <- lift $ drainFifo o
                    liftIO $ releasePort sid locks port
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

    

