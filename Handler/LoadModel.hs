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
        LoadModelCommand <$> (o .: "uuids")
                         <*> (o .:? "compiled")
    parseJSON _ = mzero

optionsLoadModelR :: SessionId -> Handler ()
optionsLoadModelR _ = do
    addHeader "Allow" "POST"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "POST"
    return ()

postLoadModelR :: SessionId -> Handler TypedContent
postLoadModelR sid = do
   (pic :: LoadModelCommand) <- requireJsonBody
   let c = fromMaybe False (loadModelCompiled pic)
   _ <- loadModel sid (loadModelUuids pic) c
   ret' <- getSessionInfo sid
   let ret = object ["data" .= ret']
   selectRep $ do
        provideRepType  mimeJson $ return ret
        provideRepType  mimeHtml $ return ret
        provideRepType  mimeText $ return ret

