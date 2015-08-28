{-# LANGUAGE ScopedTypeVariables #-}
module Handler.LoadModel where

import Import
import Helper.Shared


data LoadModelCommand =  LoadModelCommand {
    loadModelUuids      :: [String],
    loadModelCompiled   :: Maybe Bool
}

instance FromJSON LoadModelCommand where
    parseJSON (Object o) = 
        LoadModelCommand <$> (o .: "uuids") <*> (o .:? "compiled")
    parseJSON _ = mzero

postLoadModelR :: SessionId -> Handler TypedContent
postLoadModelR sid = do
   addHeader "Content-Type" "application/json"
   (pic :: LoadModelCommand) <- requireJsonBody
   let c = fromMaybe False (loadModelCompiled pic)
   _ <- loadModel sid (loadModelUuids pic) c
   -- return val
   ret' <- getSessionInfo sid
   let ret = object ["data" .= ret']
   selectRep $ do
        provideRepType  mimeJson $ return ret
        provideRepType  mimeHtml $ return ret
        provideRepType  mimeText $ return ret




optionsLoadModelR :: SessionId -> Handler ()
optionsLoadModelR _ = do
    addHeader "Allow" "POST"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET"
    return ()
