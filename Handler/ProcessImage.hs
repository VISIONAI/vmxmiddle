{-# LANGUAGE ScopedTypeVariables #-}
module Handler.ProcessImage where

import Import
import Helper.Shared
import Helper.VMXTypes


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
  addHeader "Content-Type" "application/json"
    
  deleteVMXSession sid


deleteVMXSession :: SessionId -> Handler TypedContent
deleteVMXSession sid = do
    
    -- stop process
    outski <- exitVMXServer sid

    -- remove from port map
    removeVMXSession sid

    
    return outski
    -- delete session files
    -- delVMXFolder $ "sessions/" <> sid			


