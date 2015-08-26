{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Helper.Shared
    ( drainFifo
    , getPipeResponse
    , getPortResponse
    , InputPipe
    , OutputPipe
    , makeJson
    , processImage
    , loadModel
    , waitLock
    , releasePort
    , exitVMXServer
    , removeVMXSession
    , delVMXFolder
    , addLock
    , nextPort
    , mimeJson
    , mimeHtml
    , mimeText
    , getSessionInfo
    ) where

import Import
import Data.Streaming.Network (bindPortTCP)
import Network.Socket (sClose)
import System.Random
import Control.Monad (guard)
import System.IO.Error (isDoesNotExistError)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import qualified Data.Text.IO as DT (readFile)
--import Data.Text.Encoding (decodeUtf8,encodeUtf8)
import System.Directory (removeDirectoryRecursive)
import System.IO
import Data.Aeson (encode,decode)
import GHC.IO.Handle.FD (openFileBlocking)
import Control.Exception  as Ex hiding (Handler) 
import Control.Exception.Lifted  as LX (finally) -- (catch,finally)
import Data.Map.Strict as SM (member, (!), insert,  Map) 
import Data.Map (delete)
import Helper.VMXTypes

import Data.Text.IO (hGetContents)

import Network.HTTP.Conduit
import Network.HTTP.Types.Status (status404,status500,status400)
import Data.Conduit

import Data.Conduit.List (consume)
-- import qualified Data.ByteString.Lazy.Char8 as C

type LockMap = SM.Map String (MVar Int)

type Port = Int

data VMXOutput = VMXOutput {
    vmxOutputError   :: Int,
    vmxOutputMessage :: String
}

instance FromJSON VMXOutput where
    parseJSON (Object o) = 
        VMXOutput <$> (o .: "error") <*> (o .: "message")
    parseJSON _ = mzero

instance ToJSON VMXOutput where
  toJSON (VMXOutput e m) =
    object ["error" .= e, "message" .= m]

removeVMXSession :: SessionId -> Handler ()
removeVMXSession sid = do
    App _ _ _ _ portMap' _ _ _ <- getYesod
    pm <- liftIO $ takeMVar portMap'
    liftIO $ putMVar portMap' (Data.Map.delete sid pm)
    return ()



releasePort :: SessionId -> LockMap -> Port -> IO ()
releasePort sid locks port = putMVar (locks ! sid) port >> return ()

addLock :: SessionId -> Maybe Port -> Handler Port
addLock sid mbPort= do
    App _ _ _ _ portMap' _ _ _  <- getYesod
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

--headers :: Handler ()
--headers = do
--    addHeader "Access-Control-Allow-Origin" "*"
--    addHeader "Content-Type" "application/json"

type InputPipe = FilePath
type OutputPipe = FilePath


-- sessionToPath :: SessionId -> Handler FilePath
-- sessionToPath sid = do
--     dataDir <- wwwDir
--     return $ dataDir ++ "/sessions/" ++ sid ++ "/"

mimeJson :: ContentType
mimeJson = "application/json"
mimeText :: ContentType
mimeText = "text/plain"
mimeHtml :: ContentType
mimeHtml = "text/html"


getPortResponse :: Value -> SessionId -> Handler TypedContent
getPortResponse input sessionId = do
    ret  <- getPortResponse' input sessionId
    let ret2 = (makeJson ret)
    let ret3 = decode $ L.fromChunks [C.pack ret] :: Maybe VMXOutput
    case ret3 of
      Just out -> do
        --liftIO $ print $ "out error is " ++ (show $ vmxOutputError out)
        case (vmxOutputError out) of
          0 -> do
            liftIO $ print $ ("code is 0!!!"::String)
          _ -> do
            sendResponseStatus status400 $ object [ "error" .= (vmxOutputMessage out) ]
      Nothing -> do
        sendResponseStatus status500 $ object [ "error" .= ("Cannot parse output"::String) ]

    
    --let ret3 = (parseJSON ret2) :: VMXOutput
    -- let ret3 = (decode ret2) :: Maybe VMXOutput
    --let ret3 = (decode $ ret2 ):: Maybe VMXOutput
    liftIO $ print $ show $ "Ret2 is " ++ (show ret2)
    --let mr = (decode ret) :: Maybe VMXOutput
    
    selectRep $ do
        provideRepType  mimeJson $ return ret
        provideRepType  mimeHtml $ return ret
        provideRepType  mimeText $ return ret


--portErrorHandler :: String -> Handler TypedContent
--portErrorHandler msg = error msg

getPortResponse' :: Value -> SessionId -> Handler String
getPortResponse' input sessionId = do
    App _ _ manager _ portMap' _ _ _ <- getYesod
    portMap <- do
        pm <- liftIO $ takeMVar portMap'
        if member sessionId pm
            then return pm
            else do
                liftIO $ putMVar portMap' pm
                sendResponseStatus status404 $ object [ "error" .= ("Session " ++ sessionId ++ " Not Found" :: String) ]

                
    liftIO $ putMVar portMap' portMap
    port <- liftIO $ waitLock sessionId portMap

    let path = "http://127.0.0.1:" ++ show port


    req' <- liftIO $ parseUrl path


    --let req = req' {method = "POST", requestBody = RequestBodyLBS $ LBS.pack "invalid shit"}
    let req = req' {method = "POST", requestBody = RequestBodyLBS $ encode input, checkStatus = \_ _ _ -> Nothing}
    res <- http req manager
            --`LX.catch` (\(StatusCodeException s a _) ->
            --             do
            --               liftIO $ print $ "a is " ++ (show (a !! 0))
            --               error $ "baddie" ++ (show s)) -- $ statusMessage s))
               -- _ ->
               --   do
               --     liftIO $ print "XXX4"
               --     error "other error"
           -- `LX.catch`
           -- (\(FailedConnectionException2 {}) ->
           --   do
           --     removeVMXSession sessionId
           --     error "Removed bad session")
           `LX.finally` (liftIO $ releasePort sessionId portMap port)
     


    resValue' <- responseBody res $$+- consume
    let ret = concat $ map C.unpack resValue'
    --let rp = (decode $ decodeUtf8 $ pack ret) :: Maybe VMXOutput
    --liftIO $ print $ "ret is " ++ (ret) -- show $ VMXOutput $ makeJson ret)
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


getSessionInfo :: FilePath -> Handler Value
getSessionInfo sid = do
  sp' <- fmap (++ "sessions/") wwwDir 
  mModelJson <- liftIO $ tryJust (guard . isDoesNotExistError) (DT.readFile  $ sp' ++ sid ++ "/model.json")
  case mModelJson of
    Right modelJson -> 
      return $ object ["id" .= sid, "model" .= (makeJson . unpack)  modelJson]
    Left _ -> return $ object ["id" .= sid, "model" .= Null]


