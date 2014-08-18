{-# LANGUAGE ScopedTypeVariables #-}
module Handler.EditModel where

import Import
import Helper.Shared
import Data.Aeson (decode', encode)
import Data.Aeson.Types (Result(..))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Types (status400)


optionsEditModelR :: SessionId -> Handler ()
optionsEditModelR _ = do
    addHeader "Allow" "PUT, POST"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "PUT, POST"
    return ()


data EditModelCommand = EditModelCommand {
    editModelSettings :: Value,
    editModelChanges  :: Value
} 
--TJM: Yesod just gives me "Malformed JSON" when I change settings to
--settings2 in the payload.  How can I get the parsing error to be
--returned and not something as generic as the default?
instance FromJSON EditModelCommand where
    parseJSON (Object o) = do
        EditModelCommand <$> (o .: "settings")
                         <*> (o .: "changes")
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
        Nothing -> sendResponseStatus status400 $ object ["error" .= ("Invalid VMXserver json" :: String, "json" .= response)]
    where
        command :: String
        command = "show_model"

-- RESPONDS TO /sessions/#SessionId/edit
-- a write against the current model
putEditModelR :: SessionId -> Handler String
putEditModelR sid = do
    headers
    addHeader "Content-Type" "application/json"
    (r :: Result EditModelCommand) <- parseJsonBody
    case r of
      Success eic -> do
        let req = object [ "command"  .= command
                         , "settings" .= (editModelSettings eic)
                         , "changes"  .= (editModelChanges eic)
                         ]
        response <- getPipeResponse req sid
        return response
      Error s -> sendResponseStatus status400 $ LBS.unpack $ encode $ object ["error" .= s]
    where
        command :: String
        command = "edit_model"


