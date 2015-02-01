{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : EditModel
Description : VMX Model Editing

This module contains functions for editing VMX models.
-}
module Handler.EditModel where

import Import
import Helper.Shared
import Data.Aeson.Types (Result(..))

{-|
The Edit Model Command, which contains both settings as well as model changes.
-}
data EditModelCommand = EditModelCommand {
    editModelSettings :: Value,
    editModelChanges  :: Value
} 

--TODO(TJM): Yesod just gives me "Malformed JSON" when I change
--settings to settings2 in the payload.  How can I get the parsing
--error to be returned and not something as generic as the default?
--TODO(TJM): I seem to have fixed this? by using
--EditModelReponseError, but because this is not tested, I'm not sure
--what I did there.
instance FromJSON EditModelCommand where
    parseJSON (Object o) = do
        EditModelCommand <$> (o .: "settings")
                         <*> (o .: "changes")
    parseJSON _ = mzero

{-|
The Edit Model Reponse, which contains a bunch of positives and negatives

TODO(TJM): I'm not sure how the EditModelResponseError works...
-}
data EditModelResponse = EditModelResponse Value String | EditModelResponseError Integer String

{-|
TODO(TJM): not sure what's happening here
-}
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
    parseJSON _ = mzero

{-|
Returns available REST verbs for the \/sessions\/#SessionId\/edit handler
-}
optionsEditModelR :: SessionId -> Handler ()
optionsEditModelR _ = do
    addHeader "Allow" "PUT, POST"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "PUT, POST"
    return ()

{-|
POST \/sessions\/#SessionId\/edit

Read-only and does not modify the model. Calls 'genericEditModel' with "show_model"
-}
postEditModelR :: SessionId -> Handler TypedContent
postEditModelR sid = genericEditModel sid "show_model"

{-|
PUT \/sessions\/#SessionId\/edit

This modifies the current model. Calls 'genericEditModel' with "edit_model"
-}
putEditModelR :: SessionId -> Handler TypedContent
putEditModelR sid = genericEditModel sid "edit_model"

{-|
A generic edit model command which is used for both show_model and edit_model requests
-}
genericEditModel :: SessionId -> String -> Handler TypedContent
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
        response <- getPortResponse req sid
        return response

