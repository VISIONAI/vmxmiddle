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

data EditModelResponse = EditModelResponse Value String | EditModelResponseError Integer String
editModelData :: EditModelResponse -> Value
editModelData (EditModelResponse d _ ) = d
editModelData (EditModelResponseError _ _ ) = undefined


instance ToJSON EditModelResponse where
    toJSON (EditModelResponse d m)= 
        object ["data" .= d
               , "message" .= m
               ]
    toJSON (EditModelResponseError b m)= 
        object ["error" .= b, "message" .= m]


instance FromJSON EditModelResponse where
    parseJSON (Object o) = EditModelResponse <$> (o .: "data") <*> (o .: "message")
      -- case o of
      --   EditModelResponse d m -> EditModelResponse <$> (o .: "data") <*> (o .: "message")
      --   EditModelResponseError b m -> EditModelResponseError <$> (o .: "error") <*> (o .: "message")
    parseJSON _ = mzero

-- RESPONDS TO /sessions/#SessionId/edit
-- this is a read against the current model running at the session
postEditModelR :: SessionId -> Handler Value
postEditModelR sid = genericEditModel sid "show_model"

-- RESPONDS TO /sessions/#SessionId/edit
-- a write against the current model
putEditModelR :: SessionId -> Handler Value
putEditModelR sid = genericEditModel sid "edit_model"

genericEditModel :: SessionId -> String -> Handler Value
genericEditModel sid command = do
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
        let (mbResponse :: Maybe EditModelResponse) = decode' $ LBS.pack response
        case mbResponse of
          Just response' ->
            case response' of
              EditModelResponse _ _ -> return $ toJSON response'
              EditModelResponseError _ _ -> sendResponseStatus status400 $ toJSON response'
          Nothing -> do
            sendResponseStatus status400 $ object ["error" .= response]
      Error s -> sendResponseStatus status400 $ LBS.unpack $ encode $ object ["error" .= s]


