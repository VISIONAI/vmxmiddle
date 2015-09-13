{-# LANGUAGE ScopedTypeVariables #-}

module Handler.SaveSession where

import Import
import Helper.Shared
import Data.Aeson.Types

data SaveSessionCommand = SaveSessionCommand {
    saveSessionName :: Maybe String,
    saveSessionNewUUID :: Maybe Bool
}

instance FromJSON SaveSessionCommand where
    parseJSON (Object o) =
        SaveSessionCommand <$> (o .:? "name")
                           <*> (o .:? "new_uuid")
    parseJSON _ = mzero

instance ToJSON SaveSessionCommand where
    toJSON (SaveSessionCommand name new_uuid) =
            object ["name" .= fromMaybe "" name,
                    "new_uuid" .= fromMaybe False new_uuid,
                    "command" .= ("save" :: String)]

optionsSaveSessionR :: SessionId -> Handler ()
optionsSaveSessionR _ = do
    addHeader "Allow" "Post"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "POST"
    return ()

postSaveSessionR :: SessionId -> Handler TypedContent
postSaveSessionR sid = do
    cmd2::Result SaveSessionCommand <- parseJsonBody
    let cmd = case cmd2 of
                 Error _ -> SaveSessionCommand Nothing Nothing
                 Success val -> val
    getPortResponse (toJSON cmd) sid >>= return
    
