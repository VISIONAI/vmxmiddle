{-# LANGUAGE ScopedTypeVariables #-}
module Handler.ProcessImage where

import Import
import Helper.Shared
import Helper.VMXTypes

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
        ProcessImageCommand <$> (o .:? "name")
                            <*> (o .:? "images")
                            <*> (o .:? "params")
    parseJSON _ = mzero

optionsProcessImageR :: SessionId -> Handler ()
optionsProcessImageR _ = do
    addHeader "Allow" "Get, Post"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET, POST"
    return ()

postProcessImageR :: SessionId -> Handler TypedContent
postProcessImageR sid = do
   (pic :: ProcessImageCommand) <- requireJsonBody
   let params = fromMaybe (VMXParams Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (processImageParams pic)
   let images = fromMaybe ([]) (processImageImages pic)
   let name = fromMaybe "" (processImageName pic);
   val <- processImage sid images params name
   return val
