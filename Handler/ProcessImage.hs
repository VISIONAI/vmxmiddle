{-# LANGUAGE ScopedTypeVariables #-}
module Handler.ProcessImage where

import Import
import Helper.Shared
import Helper.VMXTypes

--import System.Directory (doesFileExist)
--import Control.Concurrent (threadDelay)
--import Data.Map.Strict as SM (member) 
--import Network.HTTP.Types.Status (status404)

optionsProcessImageR :: SessionId -> Handler ()
optionsProcessImageR _ = do
    addHeader "Allow" "GET, POST, OPTIONS"
    addHeader "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
    return ()

data ProcessImageCommand =  ProcessImageCommand {
    processImageName   :: Maybe String,
    processImageImages :: Maybe [VMXImage],
    processImageParams   :: Maybe VMXParams
}

-- NOTE from TJM: "weeneedtogiveitname" is probably here because we
-- can in theory give a process image command the name of the objects
-- we only care about.. this make sense if we've loaded a bunch of
-- models into one session, but only want the responses for "hand" and
-- not the remaining "100 objects.  Currently we aren't doing this...
instance FromJSON ProcessImageCommand where
    parseJSON (Object o) = do
        ProcessImageCommand <$> (o .:? "name") <*> (o .:? "images") <*> (o .:? "params")
    parseJSON _ = mzero

postProcessImageR :: SessionId -> Handler TypedContent
postProcessImageR sid = do
   addHeader "Content-Type" "application/json"
   (pic :: ProcessImageCommand) <- requireJsonBody
   let params = fromMaybe (VMXParams Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (processImageParams pic)
   let images = fromMaybe ([]) (processImageImages pic)
   let name = fromMaybe "" (processImageName pic);
   val <- processImage sid images params name
   return val



