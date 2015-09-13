{-# LANGUAGE ScopedTypeVariables #-}
module Handler.EditModel where

import Import
import Helper.Shared

data VMXEditSettings = VMXEditSettings {
    vmxESlearn_iterations  :: Maybe Integer,
    vmxESmax_positives  :: Maybe Integer,
    vmxESmax_negatives  :: Maybe Integer,
    vmxESpositives_order  :: Maybe Integer,
    vmxESnegatives_order  :: Maybe Integer,
    vmxESpad_scale :: Maybe Float,
    vmxESimage_size :: Maybe Integer
}

instance ToJSON VMXEditSettings where
    toJSON (VMXEditSettings lp maxp maxn po no ps is) =
            object ["learn_iterations" .= fromMaybe 0 lp,
                    "max_positives" .= fromMaybe 20 maxp,
                    "max_negatives" .= fromMaybe 20 maxn,
                    "positives_order" .= fromMaybe 1 po,
                    "negatives_order" .= fromMaybe (-1) no,
                    "pad_scale" .= fromMaybe (1.0) ps,
                    "image_size" .= fromMaybe (100) is]

instance FromJSON VMXEditSettings where
    parseJSON (Object o) =
        VMXEditSettings <$> (o .:? "learn_iterations")
        <*> (o .:? "max_positives")
        <*> (o .:? "max_negatives")
        <*> (o .:? "positives_order")
        <*> (o .:? "negatives_order")
        <*> (o .:? "pad_scale")
        <*> (o .:? "image_size")
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

instance FromJSON EditModelCommand where
    parseJSON (Object o) = do
        EditModelCommand <$> (o .: "settings")
                         <*> (o .: "changes")
    parseJSON _ = mzero

genericEditModel :: SessionId -> String -> Handler TypedContent
genericEditModel sid command = do
    (r :: EditModelCommand) <- requireJsonBody
    let req = object [ "command"  .= command
                     , "settings" .= (editModelSettings r)
                     , "changes"  .= (editModelChanges r)
                     ]
    response <- getPortResponse req sid
    return response

optionsEditModelR :: SessionId -> Handler ()
optionsEditModelR _ = do
    addHeader "Allow" "POST"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "POST"
    return ()

postEditModelR :: SessionId -> Handler TypedContent
postEditModelR sid = genericEditModel sid "edit"

