{-# LANGUAGE ScopedTypeVariables #-}
module Handler.SaveSession where

import Import
import Helper.Shared

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
                    "command" .= ("save_model" :: String)]
 
postSaveSessionR :: SessionId -> Handler TypedContent
postSaveSessionR sid = do
    --liftIO $ print "About to get json body"
    cmd :: SaveSessionCommand <- requireJsonBody
    --liftIO $ print $ ("cmd is here"::String) ++ (show $ toJSON cmd)
    response <- getPortResponse (toJSON cmd) sid
    return response


