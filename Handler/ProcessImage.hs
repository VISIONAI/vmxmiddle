{-# LANGUAGE ScopedTypeVariables #-}
module Handler.ProcessImage where

import Import
import Helper.Shared
import Helper.VMXTypes

import System.Directory (doesFileExist)
import Control.Concurrent (threadDelay)
import Data.Map.Strict as SM (member) 

optionsProcessImageR :: SessionId -> Handler ()
optionsProcessImageR _ = do
    addHeader "Allow" "GET, POST, DELETE, OPTIONS"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type,Origin, X-Requested-With, Accept"
    addHeader "Access-Control-Allow-Methods" "GET, POST, DELETE, OPTIONS"
    return ()

data ProcessImageCommand =  ProcessImageCommand {
    processImageName   :: String,
    processImageImages :: [VMXImage],
    processImageParams   :: Maybe VMXParams
}

-- NOTE from TJM: "weeneedtogiveitname" is probably here because we
-- can in theory give a process image command the name of the objects
-- we only care about.. this make sense if we've loaded a bunch of
-- models into one session, but only want the responses for "hand" and
-- not the remaining "100 objects.  Currently we aren't doing this...
instance FromJSON ProcessImageCommand where
    parseJSON (Object o) = do
        ProcessImageCommand "" <$> (o .: "images") <*> (o .:? "params")
    parseJSON _ = mzero

getProcessImageR :: SessionId -> Handler TypedContent
getProcessImageR sid = do
  addHeader "Access-Control-Allow-Origin" "*"

  App _ _ _ _ portMap' _ _ _ <- getYesod
  _ <- do
    pm <- liftIO $ takeMVar portMap'
    if member sid pm
      then do
        liftIO $ putMVar portMap' pm
        return pm
      else do
        liftIO $ putMVar portMap' pm
        notFound -- [pack $ "invalid session " ++ sessionId ]

  
  resid <- getSessionInfo sid
  let response = object ["data" .= resid]
  selectRep $ do
    provideRepType  mimeJson $ return response
    provideRepType  mimeHtml $ return response
    provideRepType  mimeText $ return response

  --return $ object ["data" .= resid]

postProcessImageR :: SessionId -> Handler TypedContent
postProcessImageR sid = do
   addHeader "Access-Control-Allow-Origin" "*"
   addHeader "Content-Type" "application/json"
   (pic :: ProcessImageCommand) <- requireJsonBody
   let params = fromMaybe (VMXParams Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (processImageParams pic)
   val <- processImage sid (processImageImages pic) params (processImageName pic)
   return val

deleteProcessImageR :: SessionId -> Handler TypedContent
deleteProcessImageR sid = do
  addHeader "Access-Control-Allow-Origin" "*"
  -- addHeader "Content-Type" "application/json"
    
  response <- deleteVMXSession sid
  selectRep $ do
    provideRepType  mimeJson $ return response
    provideRepType  mimeHtml $ return response
    provideRepType  mimeText $ return response



deleteVMXSession :: SessionId -> Handler Value
deleteVMXSession sid = do
    
    -- stop process
    _ <- exitVMXServer sid

    -- remove from port map
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
            


