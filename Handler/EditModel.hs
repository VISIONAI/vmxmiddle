{-# LANGUAGE ScopedTypeVariables #-}
module Handler.EditModel where

import Import
import Helper.Shared
--import Data.Aeson (decode', encode)
--import Data.Aeson.Types (Result(..))
--import qualified Data.ByteString.Lazy.Char8 as LBS
--import Network.HTTP.Types (status400)

optionsEditModelR :: SessionId -> Handler ()
optionsEditModelR _ = do
    addHeader "Allow" "POST"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "POST"
    return ()

data VMXEditSettings = VMXEditSettings {
    vmxESlearn_iterations  :: Maybe Integer,
    vmxESmax_positives  :: Maybe Integer,
    vmxESmax_negatives  :: Maybe Integer,
    vmxESpositives_order  :: Maybe Integer,
    vmxESnegatives_order  :: Maybe Integer
}

instance ToJSON VMXEditSettings where
    toJSON (VMXEditSettings lp maxp maxn po no) =
            object ["learn_iterations" .= fromMaybe 0 lp,
                    "max_positives" .= fromMaybe 20 maxp,
                    "max_negatives" .= fromMaybe 20 maxn,
                    "positives_order" .= fromMaybe 1 po,
                    "negatives_order" .= fromMaybe (-1) no]

instance FromJSON VMXEditSettings where
    parseJSON (Object o) =
        VMXEditSettings <$> (o .:? "learn_iterations")
        <*> (o .:? "max_positives")
        <*> (o .:? "max_negatives")
        <*> (o .:? "positives_order")
        <*> (o .:? "negatives_order")
        
    parseJSON _ = mzero


data VMXEditChange = VMXEditChange {
    vmxECimage  :: Maybe String,
    vmxESscore  :: Maybe Double,
    vmxESclass_label  :: Integer,
    vmxESid  :: Integer,
    vmxEStime  :: Maybe String,
    vmxESdata  :: Maybe [Float]
}

instance ToJSON VMXEditChange where
    toJSON (VMXEditChange image score class_label curid time curdata) =
            object ["image" .= image,
                    "score" .= score,
                    "class_label" .= class_label,
                    "id" .= curid,
                    "time" .= time,
                    "data" .= curdata]

instance FromJSON VMXEditChange where
    parseJSON (Object o) =
        VMXEditChange <$> (o .:? "image")
        <*> (o .:? "score")
        <*> (o .: "class_label")
        <*> (o .: "id")
        <*> (o .:? "time")
        <*> (o .:? "data")
        
    parseJSON _ = mzero



data EditModelCommand = EditModelCommand {
    editModelSettings :: Maybe VMXEditSettings,
    editModelChanges  :: [VMXEditChange]
} 
--TJM: Yesod just gives me "Malformed JSON" when I change settings to
--settings2 in the payload.  How can I get the parsing error to be
--returned and not something as generic as the default?
instance FromJSON EditModelCommand where
    parseJSON (Object o) = do
        EditModelCommand <$> (o .: "settings")
                         <*> (o .: "changes")
    parseJSON _ = mzero

-- data EditModelResponse = EditModelResponse Value String | EditModelResponseError Integer String
-- editModelData :: EditModelResponse -> Value
-- editModelData (EditModelResponse d _ ) = d
-- editModelData (EditModelResponseError _ _ ) = undefined


-- instance ToJSON EditModelResponse where
--     toJSON (EditModelResponse d m)= 
--         object ["data" .= d
--                , "message" .= m
--                ]
--     toJSON (EditModelResponseError b m)= 
--         object ["error" .= b, "message" .= m]


-- instance FromJSON EditModelResponse where
--     parseJSON (Object o) = EditModelResponse <$> (o .: "data") <*> (o .: "message")
--       -- case o of
--       --   EditModelResponse d m -> EditModelResponse <$> (o .: "data") <*> (o .: "message")
--       --   EditModelResponseError b m -> EditModelResponseError <$> (o .: "error") <*> (o .: "message")
--     parseJSON _ = mzero

-- RESPONDS TO /sessions/#SessionId/edit
-- this is a read against the current model running at the session
postEditModelR :: SessionId -> Handler TypedContent
postEditModelR sid = genericEditModel sid "edit_model"

-- RESPONDS TO /sessions/#SessionId/edit
-- a write against the current model
--putEditModelR :: SessionId -> Handler TypedContent
--putEditModelR sid = genericEditModel sid "edit_model"

genericEditModel :: SessionId -> String -> Handler TypedContent
genericEditModel sid command = do
    addHeader "Content-Type" "application/json"
    (r :: EditModelCommand) <- requireJsonBody
    let req = object [ "command"  .= command
                     , "settings" .= (editModelSettings r)
                     , "changes"  .= (editModelChanges r)
                     ]
    response <- getPortResponse req sid
    return response
