{-# LANGUAGE ScopedTypeVariables #-}
module Handler.ManageSession where

import Import
import Helper.Shared
import Helper.VMXTypes

import System.Directory (doesFileExist)
import Control.Concurrent (threadDelay)
import Data.Map.Strict as SM (member) 
import Network.HTTP.Types.Status (status404)

data ManageSessionCommand =  ManageSessionCommand {
    processImageName   :: Maybe String,
    processImageImages :: Maybe [VMXImage],
    processImageParams   :: Maybe VMXParams
}

-- NOTE from TJM: "weeneedtogiveitname" is probably here because we
-- can in theory give a process image command the name of the objects
-- we only care about.. this make sense if we've loaded a bunch of
-- models into one session, but only want the responses for "hand" and
-- not the remaining "100 objects.  Currently we aren't doing this...
instance FromJSON ManageSessionCommand where
    parseJSON (Object o) = do
        ManageSessionCommand <$> (o .:? "name")
                             <*> (o .:? "images")
                             <*> (o .:? "params")
    parseJSON _ = mzero

optionsManageSessionR :: SessionId -> Handler ()
optionsManageSessionR _ = do
    addHeader "Allow" "Get, Post, Delete"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET, POST, DELETE"
    return ()

getManageSessionR :: SessionId -> Handler TypedContent
getManageSessionR sid = do  
  App _ _ _ _ portMap' _ _ _ <- getYesod
  _ <- do
    pm <- liftIO $ takeMVar portMap'
    if member sid pm
      then do
        liftIO $ putMVar portMap' pm
        return pm
      else do
        liftIO $ putMVar portMap' pm
        sendResponseStatus status404 $ object [ "error" .= ("Session " ++ sid ++ " Not Found" :: String) ]

  
  resid <- getSessionInfo sid
  let response = object ["data" .= resid]
  selectRep $ do
    provideRepType  mimeJson $ return response
    provideRepType  mimeHtml $ return response
    provideRepType  mimeText $ return response

postManageSessionR :: SessionId -> Handler TypedContent
postManageSessionR sid = do
   (pic :: ManageSessionCommand) <- requireJsonBody
   let params = fromMaybe (VMXParams Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (processImageParams pic)
   let images = fromMaybe ([]) (processImageImages pic)
   let name = fromMaybe "" (processImageName pic)
   val <- processImage sid images params name
   return val

deleteManageSessionR :: SessionId -> Handler TypedContent
deleteManageSessionR sid = do    
  response <- deleteVMXSession sid
  selectRep $ do
    provideRepType  mimeJson $ return response
    provideRepType  mimeHtml $ return response
    provideRepType  mimeText $ return response

deleteVMXSession :: SessionId -> Handler Value
deleteVMXSession sid = do
    _ <- exitVMXServer sid
    removeVMXSession sid
    sessionPath'    <- sessionPath sid
    liftIO $ waitForFileGone (sessionPath' ++ "/url")
    let o = object ["id" .= sid]
    return $ object ["data" .= o]
    -- delete session files
    -- delVMXFolder $ "sessions/" <> sid
    where
      sessionPath :: SessionId -> Handler String
      sessionPath s = do
        dir <- wwwDir 
        return $ dir ++ "sessions/" ++ s
      waitForFileGone :: FilePath -> IO ()
      waitForFileGone f = do
        --liftIO $ print $ "Checking to see if " ++ f ++ " is still there"
        stillThere <- doesFileExist f
        case stillThere of
          False -> return () --liftIO $ print "good"
          True -> do
            threadDelay 200
            waitForFileGone f
            


