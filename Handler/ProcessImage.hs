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
   waitLock sid
   addHeader "Access-Control-Allow-Origin" "*"
   addHeader "Content-Type" "application/json"
   (pic :: ProcessImageCommand) <- requireJsonBody
   let req = object ["command" .= command, "image" .= picImage pic, "params" .= processImageParams pic, "time" .= picTime pic]
   response <- getPipeResponse req sid
   releaseLock sid
   return response
   where
        command :: String
        command = "process_image"

