{-# LANGUAGE ScopedTypeVariables #-}
module Handler.EditModel where

import Import
import Helper.Shared
import Data.Aeson (decode')
import qualified Data.ByteString.Lazy.Char8 as LBS

optionsEditModelR :: SessionId -> Handler ()
optionsEditModelR _ = do
    addHeader "Allow" "PUT, POST"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "PUT, POST"
    return ()


data EditModelCommand = EditModelCommand {
    editModelSettings :: Value,
    editModelChanges  :: Maybe Value
} 

instance FromJSON EditModelCommand where
    parseJSON (Object o) = do
        EditModelCommand <$> (o .: "settings")
                         <*> (o .:? "changes")
    parseJSON _ = mzero

data EditModelResponse = EditModelResponse {
    editModelData :: Value,
    editModelMessage :: String
}

instance FromJSON EditModelResponse where
    parseJSON (Object o) = EditModelResponse <$> (o .: "data") <*> (o .: "message")
    parseJSON _ = mzero

-- RESPONDS TO /sessions/#SessionId/edit
-- this is a read against the current model running at the session
postEditModelR :: SessionId -> Handler Value
postEditModelR sid = do
    headers
    (eic :: EditModelCommand) <- requireJsonBody
    let req = object ["command" .= command, "settings" .= (editModelSettings eic)]
    response <- getPipeResponse req sid
    let response' = decode' $ LBS.pack response
    case response' of
        Just x -> return $ object ["data" .= editModelData x]
        Nothing -> do
            return $ object ["error" .= ("invalid matlab json" :: String, "json" .= response)]
    where
        command :: String
        command = "show_model"

-- RESPONDS TO /sessions/#SessionId/edit
-- a write against the current model
putEditModelR :: SessionId -> Handler String
putEditModelR sid = do
    headers
    addHeader "Content-Type" "application/json"
    (eic :: EditModelCommand) <- requireJsonBody
    let req = object ["command" .= command, "settings" .= (editModelSettings eic), "changes" .= (fromMaybe (object []) $ editModelChanges eic)]
    response <- getPipeResponse req sid
    return response
    where
        command :: String
        command = "edit_model"


