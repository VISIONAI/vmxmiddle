{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Helper.Shared
    ( drainFifo
    , headers
    , getPipeResponse
    , getPortResponse
    , getPortResponse'
    , InputPipe
    , OutputPipe
    , makeJson
    , processImage
    , loadModel
    , waitLock
    , releasePort
    , exitVMXServer
    , delVMXFolder
    , addLock
    , nextPort
    ) where

import Import
import Data.Streaming.Network (bindPortTCP)
import Network.Socket (sClose)
import System.Random
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import System.Directory (removeDirectoryRecursive)
import System.IO
import Data.Aeson (encode)
import GHC.IO.Handle.FD (openFileBlocking)
import Control.Exception  as Ex hiding (Handler) 

import Data.Map.Strict as SM (member, (!), insert,  Map) 

import Helper.VMXTypes

import Data.Text.IO (hGetContents)




import Network.HTTP.Conduit
import Data.Conduit

import Data.Conduit.List (consume)

type LockMap = SM.Map String (MVar Int)

type Port = Int

releasePort :: SessionId -> LockMap -> Port -> IO ()
releasePort sid locks port = putMVar (locks ! sid) port >> return ()

addLock :: SessionId -> Maybe Port -> Handler Port
addLock sid mbPort= do
    App _ _ _ _ portMap' _ _  <- getYesod
    portMap <- liftIO $ takeMVar portMap'
    (port, newMap) <- case mbPort of 
                Just requestedPort -> do  
                    if (member sid portMap) 
                        then do -- requested specific port, but we already have one
                            existingPort <- liftIO $ readMVar (portMap ! sid)
                            if existingPort == requestedPort 
                                then return (requestedPort, portMap) 
                                else error "requesting a different port than current port"
                        else do -- requested a specific Port and we don't have one
                            newLock <- liftIO $ newMVar requestedPort
                            return $ (requestedPort, SM.insert sid newLock portMap)
                Nothing ->
                    if (member sid portMap) --we already have a port, but lets just give it to them
                        then do 
                            existingPort <- liftIO $ readMVar (portMap ! sid)
                            return (existingPort, portMap)
                    else do
                        newPort <- nextPort
                        newLock <- liftIO $ newMVar newPort
                        return $ (newPort, SM.insert sid newLock portMap)
    liftIO $ putMVar portMap' newMap
    return port
            
            

nextPort :: Handler Int
nextPort = do
    aPort <- liftIO $ randomRIO (1025, 65000)
    valid <- liftIO $ checkPort aPort
    if valid
        then return aPort
        else nextPort

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


sessionToPath :: SessionId -> Handler FilePath
sessionToPath sid = do
    dataDir <- wwwDir
    return $ dataDir ++ "/sessions/" ++ sid ++ "/"

getPortResponse :: Value -> SessionId -> Handler TypedContent
getPortResponse input sessionId = do
    ret <- getPortResponse' input sessionId
    selectRep $ do
        provideRepType  mimeJson $ return ret
        provideRepType  mimeHtml $ return ret
        provideRepType  mimeText $ return ret

mimeJson :: ContentType
mimeJson = "application/json"
mimeText = "text/plain"
mimeHtml = "text/html"

portErrorHandler :: String -> Handler TypedContent
portErrorHandler msg = error msg


getPortResponse' :: Value -> SessionId -> Handler String
getPortResponse' input sessionId = do
    App _ _ manager _ portMap' _ _ <- getYesod
    portMap <- do
        pm <- liftIO $ takeMVar portMap'
        if member sessionId pm
            then return pm
            else do
                liftIO $ putMVar portMap' pm
                invalidArgs [pack sessionId, "invalid session"]
                
    liftIO $ putMVar portMap' portMap
    port <- liftIO $ waitLock sessionId portMap

    let path = "http://127.0.0.1:" ++ show port ++ "/command"


    req' <- liftIO $ parseUrl path


    --let req = req' {method = "POST", requestBody = RequestBodyLBS $ LBS.pack "invalid shit"}
    let req = req' {method = "POST", requestBody = RequestBodyLBS $ encode input}
    res <- http req manager
    resValue <- responseBody res $$+- consume
    liftIO $ releasePort sessionId portMap port

    let ret = concat $ map C.unpack resValue
    return ret



checkPort :: Int -> IO Bool
checkPort p = do
    es <- Ex.try $ bindPortTCP p "*4"
    case es of
        Left (_ :: Ex.IOException) -> return False
        Right s -> do
            sClose s
            return True

    
getPipeResponse :: Value -> SessionId -> Handler TypedContent
getPipeResponse = getPortResponse
--getPipeResponse v sid = do
--    App _ _ _ _ lm _ _  <- getYesod
--    locks' <- liftIO $ takeMVar lm
--
--    -- make sure we have a lock for this one
--    locks <- if member sid locks'
--                    then return locks'
--                    else addLock lm sid
--
--
--    port <- liftIO  $ waitLock sid locks
--    liftIO  $ putMVar lm locks
--    i    <- getInputPipe  sid
--    o    <- getOutputPipe sid
--    fileE <-  lift $ try (openFileBlocking i WriteMode)
--    case fileE of
--                -- An IOError here means the process that was supposed
--                -- to read from the pipe died before it could, and matlab
--                -- won't read again until we eat its (now useless) output
--                Left (e :: IOError)->   do
--                    _ <- lift $ drainFifo o
--                    liftIO  $ releasePort sid locks port
--                    error $ show e
--                    --lift $ openFileBlocking i WriteMode >>= return
--                Right fileHandle -> do
--                    liftIO $ L.hPutStr fileHandle payload
--                    lift $ hClose fileHandle
--                    ret <- lift $ drainFifo o
--                    liftIO $ releasePort sid locks port
--                    return ret
--    where
--        payload = encode v



    
    





processImage :: SessionId -> [VMXImage] -> VMXParams -> String -> Handler TypedContent
processImage sid image params name = do
   let req = object ["command" .= command, "name" .= name, "images" .= image, "params" .= params]
   response <- getPipeResponse req sid
   return response
   where
        command :: String
        command = "process_image"

loadModel :: SessionId -> [String] -> Bool -> Handler TypedContent
loadModel sid uuids compiled = do
   let req = object ["command" .= command, "uuids" .= uuids, "compiled" .= compiled]
   response <- getPipeResponse req sid
   return response
   where
        command :: String
        command = "load_model"


exitVMXServer :: SessionId -> Handler TypedContent
exitVMXServer sid = getPipeResponse (object ["command" .= exit]) sid >>=  return
	where 	
		exit :: String
		exit = "exit"

delVMXFolder :: FilePath -> Handler ()
delVMXFolder fp = do
	wwwDir' <- wwwDir
	liftIO $ removeDirectoryRecursive $ wwwDir' <> fp >>= return
    

    
--instance WebSocketsData Value where
--   toLazyByteString v = encode v
--   fromLazyByteString s = do
--        case decode s of
--            Just val -> val
--            Nothing -> object []





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

    

