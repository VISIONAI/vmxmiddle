{-# LANGUAGE ScopedTypeVariables #-}
module Handler.ProcessImage where

import Import
import Helper.Shared
import Helper.VMXTypes

import Data.Map.Strict as SM (member)

optionsProcessImageR :: SessionId -> Handler ()
optionsProcessImageR _ = do
    addHeader "Allow" "GET POST DELETE"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET POST DELETE"
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
  App _ _ _ _ portMap' _ _ <- getYesod
  _ <- do
        pm <- liftIO $ takeMVar portMap'
        if member sid pm
            then return pm
            else do
                liftIO $ putMVar portMap' pm
                invalidArgs [pack $ "invalid session " ++ sid ]
                
  ret <- getSessionInfo sid
  selectRep $ do
    provideRepType  mimeJson $ return ret
    provideRepType  mimeHtml $ return ret
    provideRepType  mimeText $ return ret


postProcessImageR :: SessionId -> Handler TypedContent
postProcessImageR sid = do
   addHeader "Access-Control-Allow-Origin" "*"
   -- addHeader "Content-Type" "application/json"
   (pic :: ProcessImageCommand) <- requireJsonBody
   let params = fromMaybe (VMXParams Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (processImageParams pic)
   val <- processImage sid (processImageImages pic) params (processImageName pic)
   return val

deleteProcessImageR :: SessionId -> Handler ()
deleteProcessImageR = deleteVMXSession


deleteVMXSession :: SessionId -> Handler ()
deleteVMXSession sid = do
    
    -- stop process
    _ <- exitVMXServer sid

    -- remove from port map
    removeVMXSession sid

    
    return ()
    -- delete session files
    -- delVMXFolder $ "sessions/" <> sid			


