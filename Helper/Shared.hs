{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Shared
Description : VMX Shared Functions

Common utility functions
-}
module Helper.Shared
    ( headers
    , getPortResponse
    , waitLock
    , releasePort
    , addLock
    , nextPort
    , getRecursiveContents
    , pickRandom
    , fileEnding
    , last9  
    ) where

import Import
import Data.Streaming.Network (bindPortTCP)
import Network.Socket (sClose)
import System.Random
--import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.Aeson (encode)
import Control.Exception  as Ex hiding (Handler) 
import Data.Map.Strict as SM (member, (!), insert,  Map) 
import Network.HTTP.Conduit
import Data.Conduit
import Data.Conduit.List (consume)
import System.FilePath ((</>))
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)

type LockMap = SM.Map String (MVar Int)
type Port = Int

{-|
Release a port from the internal list of used ports so that it can be used again.
-}
releasePort :: SessionId -> LockMap -> Port -> IO ()
releasePort sid locks port = putMVar (locks ! sid) port >> return ()

{-|
Add a session lock to the internal list of locks
-}
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

{-|
Randomly generate the next available port in the range (1025,65000)
-}
nextPort :: Handler Int
nextPort = do
    aPort <- liftIO $ randomRIO (1025, 65000)
    valid <- liftIO $ checkPort aPort
    if valid
        then return aPort
        else nextPort

{-|
Wait on a lock
-}
waitLock :: SessionId -> LockMap -> IO Int
waitLock sid locks = takeMVar (locks ! sid) >>= return

{-|
TODO(TJM): why is this here? Calling this 'headers' will be confusing as this is a generic term.  How about 'VMXheaders'?
-}
headers :: Handler ()
headers = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Content-Type" "application/json"

{-|
Internal function to get the VMXserver reponse
-}
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
    let req = req' {method = "POST", requestBody = RequestBodyLBS $ encode input}
    res <- http req manager
    resValue <- responseBody res $$+- consume
    liftIO $ releasePort sessionId portMap port

    let ret = concat $ map C.unpack resValue
    return ret

mimeJson :: ContentType
mimeJson = "application/json"
mimeText :: ContentType
mimeText = "text/plain"
mimeHtml :: ContentType
mimeHtml = "text/html"

{-|
Internal function to get the VMXserver reponse, will return Json, HTML, or text
-}
getPortResponse :: Value -> SessionId -> Handler TypedContent
getPortResponse input sessionId = do
    ret <- getPortResponse' input sessionId
    selectRep $ do
        provideRepType  mimeJson $ return ret
        provideRepType  mimeHtml $ return ret
        provideRepType  mimeText $ return ret
{-|
Check if a port is available, by opening it, then closing it
-}
checkPort :: Int -> IO Bool
checkPort p = do
    es <- Ex.try $ bindPortTCP p "*4"
    case es of
        Left (_ :: Ex.IOException) -> return False
        Right s -> do
            sClose s
            return True

{-|
Recursively list files (skipping .)
-}
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

{-|
Pick a random element
-}
pickRandom :: [a] -> IO a
pickRandom xs = do
  index <- randomRIO (0, (length xs - 1))
  return (xs !! index)

{-|
Returns the last 4 characters of a string
-}
fileEnding :: String -> String
fileEnding x = reverse $ take 4 $ reverse x

{-|
Return the last 9 characters of a string
-}
last9 :: forall a. [a] -> [a]
last9 x = reverse $ take 9 $ reverse x

