{-# LANGUAGE ScopedTypeVariables #-}
module Handler.ProcessImage where

import Import
import Helper.Shared

optionsProcessImageR :: SessionId -> Handler ()
optionsProcessImageR _ = do
    addHeader "Allow" "POST"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "POST"
    return ()

data ProcessImageCommand =  ProcessImageCommand {
    picImage :: String,
    processImageParams   :: Value,
    picTime :: Int
}

instance FromJSON ProcessImageCommand where
    parseJSON (Object o) = do
        ProcessImageCommand <$> (o .: "image") <*> (o .: "params") <*> (o .: "time")
    parseJSON _ = mzero

postProcessImageR :: SessionId -> Handler String
postProcessImageR sid = do
   addHeader "Access-Control-Allow-Origin" "*"
   addHeader "Content-Type" "application/json"
   (pic :: ProcessImageCommand) <- requireJsonBody
   processImage sid (picImage pic) (processImageParams pic) (picTime pic) >>= return

