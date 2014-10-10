{-# LANGUAGE ScopedTypeVariables #-}
module Handler.LoadModel where

import Import
import Helper.Shared
import Helper.VMXTypes

data LoadModelCommand =  LoadModelCommand {
    loadModelUuids      :: [String],
    loadModelCompiled   :: Bool
}

instance FromJSON LoadModelCommand where
    parseJSON (Object o) = 
        LoadModelCommand <$> (o .: "uuids") <*> (o .: "compiled")
    parseJSON _ = mzero

postLoadModelR :: SessionId -> Handler String
postLoadModelR sid = do
   addHeader "Access-Control-Allow-Origin" "*"
   addHeader "Content-Type" "application/json"
   (pic :: LoadModelCommand) <- requireJsonBody
   val <- loadModel sid (loadModelUuids pic) (loadModelCompiled pic)
   return val



optionsLoadModelR :: SessionId -> Handler ()
optionsLoadModelR _ = do
    addHeader "Allow" "POST"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET"
    return ()
