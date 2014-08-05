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
    ) where

import Import
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import System.Directory (getDirectoryContents, createDirectory, removeDirectoryRecursive)
import System.Process
import System.IO
import Control.Exception (evaluate)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson (encode,decode)
import GHC.IO.Handle.FD (openFileBlocking)
import Yesod.WebSockets

import Data.Map.Strict as Map (member, (!), insert) 
import Data.IORef (atomicModifyIORef', readIORef)

import Control.Exception (try)
import System.IO.Error
import Helper.VMXTypes

releaseLock :: SessionId -> Handler ()
releaseLock sid = do
    App {..} <- getYesod
    currentLocks <- liftIO $ readIORef pipeLocks
    liftIO $ takeMVar (currentLocks ! sid)

waitLock :: SessionId -> Handler ()
waitLock sid = do
    App {..}   <- getYesod
    currentLocks <- liftIO $ readIORef pipeLocks
    locks <- if (member sid currentLocks)
                        then return currentLocks
                        else do
                            -- no semaphores exist for this session id
                            l <- liftIO $ newEmptyMVar
                            newLocks <- liftIO $ atomicModifyIORef' pipeLocks $
                                    \lks -> do
                                        -- update our list of semaphores to include the one we made for this sid
                                        let locks' = Map.insert sid l lks
                                        (locks',locks')
                            return newLocks

    let lock = locks ! sid
    liftIO $ putMVar lock ()
    return ()
            


-- we use drainFifo instead of a normal readFile because Haskell's non-blocking IO treats FIFOs wrong
drainFifo :: FilePath -> IO String
drainFifo f = do
    (i, out, e, p) <- runInteractiveProcess "bash" ["-c", "cat<"  <>  f] Nothing (Just [])
    hClose i
    hClose e
    hSetBinaryMode out False
    out' <- Control.Exception.evaluate (hGetContents out >>= \x -> length x `seq` return x)
    out'' <- out'
    terminateProcess p
    return out''

headers :: Handler ()
headers = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Content-Type" "application/json"

type InputPipe = FilePath
type OutputPipe = FilePath

getPipeResponse :: Value -> SessionId -> Handler String
getPipeResponse v sid = do
    waitLock sid
    i    <- getInputPipe  sid
    o    <- getOutputPipe sid
    fileE <-  lift $ try (openFile i WriteMode)
    file <- case fileE of
                -- An IOError here means the process that was supposed
                -- to read from the pipe died before it could, and matlab
                -- won't read again until we eat its (now useless) output
                Left (e :: IOError)->   do
                    _ <- lift $ drainFifo o
                    lift $ openFile i WriteMode >>= return
                Right file -> return file
    lift $ hPutStr file payload
    lift $ hClose file
    ret <- lift $ drainFifo o
    releaseLock sid
    return ret
    where
        payload = LBS.unpack $ encode v

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

    

