{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Model where

import Import
import Helper.Shared
import Control.Monad (filterM)
import Data.Aeson (decode, withObject, encode)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import           Data.Typeable
import           GHC.Generics
import           Data.Data
import           Helper.VMXTypes
import           System.Directory (getDirectoryContents,doesFileExist)
import qualified Data.Text.IO as DT (readFile)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Handler.ModelDB
import Helper.Redis

optionsModelR :: Handler ()
optionsModelR = do
    addHeader "Allow" "Get, Put, Post"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET, PUT, POST"
    return ()


--list all models
getModelR :: Handler TypedContent
getModelR = getModelDBR
--    addHeader "Access-Control-Allow-Origin" "*"
--    ret <- list_models
--    selectRep $ do
--        provideRepType  mimeJson $ return ret
--        provideRepType  mimeHtml $ return $ ("<pre>" <> (decodeUtf8 $ encodePretty $ ret) <> "</pre>")
--        provideRepType  mimeText $ return ret

data SaveModelCommand = SaveModelCommand {
    saveModelSid :: Text,
    saveModelName :: Text,
    saveModelNewUUID :: Maybe Bool
                     
}

instance FromJSON SaveModelCommand where
    parseJSON (Object o) =
        SaveModelCommand <$> (o .: "session_id")
        <*> (o .: "name")
        <*> (o .:? "new_uuid")
    parseJSON _ = mzero

-- save the model
putModelR :: Handler TypedContent
putModelR =  do
    mId <- maybeAuthId
    headers
    cmd :: SaveModelCommand <- requireJsonBody
    let sid = saveModelSid cmd
    let name = saveModelName cmd
    let new_uuid = saveModelNewUUID cmd

    let req = object ["command" .= save_model, "name" .= name, "new_uuid" .= new_uuid]
    _ <- getPortResponse req sid
    ret <- getSessionInfo sid
    selectRep $ do
        provideRepType  mimeJson $ return ret
        provideRepType  mimeHtml $ return ret
        provideRepType  mimeText $ return ret

    where
        save_model :: String = "save_model"

data CreateModelCommand = CreateModelCommand {
    createModelName :: Text,
    createModelParams :: Value,
    createModelImages :: [VMXImage],
    createModelSid :: Text
}


-- { "error": 0, "message": "Create Model Success (UUID=oneeye)", "warning": "Create Model (Model Not Saved)", "data": { "model": { "uuid": "1356ff35-e234-4487-8840-7d8ec4207d4b", "name": "oneeye", "size": [ 4, 4 ], "num_pos": 1, "num_neg": 86, "start_time": "2015-02-21T03:19:42.991Z", "end_time": "2015-02-21T03:19:42.991Z" }, "time": 2.3303579999999999 } }




type ModelName = String

instance FromJSON CreateModelCommand where
    parseJSON (Object o) = do
        CreateModelCommand <$> (o .: "name") <*> (o .: "params") <*> (o .: "images") <*> (o .: "session_id")
    parseJSON _ = mzero





--create new model
postModelR :: Handler TypedContent
postModelR = do
    addHeader "Access-Control-Allow-Origin" "*"
    headers

    cmc <- requireJsonBody
    mAuthId <- maybeAuthId
    let sid = createModelSid cmc
    wwwDir' <- wwwDir
    let images = createModelImages cmc
    let req = object ["name"       .= createModelName cmc,
                      "params"     .= createModelParams cmc,
                      "images"     .= images,
                      "command"    .= ("create_model" :: String)
                     ]
    response <- getPortResponse' req sid
    liftIO $ print response
    let a :: Maybe CreateModelResponse = decode $  L.fromChunks $ [C.pack response]
    case a of 
        Just (CreateModelResponse _ (CreateModelData vmxmodel@(VMXModel uuid name size pos neg start end))) -> do
            _ <- runDB $ insert $ Model mAuthId (pack uuid) (pack name) size pos neg start end (image_url uuid)
            _ <- setSessionInfo sid vmxmodel
            returnReps' $ object ["id" .= sid, "model" .= vmxmodel]
        _ -> error "could not decode"
    where
        image_url uuid = "models/" <> pack uuid <> "/image.jpg"


data ListModelResponse = ListModelResponse {
    listModelName :: String,
    listModelMeta :: String,
    listModelSize :: [Int],
    listModelHistory :: [String],
    listModelNumPos   :: Int,
    listModelNumNeg   :: Int,
    listModelStartTime :: String,
    listModelEndTime   :: String,
    listModelUUID      :: String
}


instance FromJSON ListModelResponse where
    parseJSON (Object o) = do
        ListModelResponse <$> (o .: "name")
                         <*> (o .: "meta")
                         <*> (o .: "size")
                         <*> (o .: "history")
                         <*> (o .: "num_pos")
                         <*> (o .: "num_neg")
                         <*> (o .: "start_time")
                         <*> (o .: "end_time")
                         <*> (o .: "uuids")
    parseJSON _ = mzero

instance FromJSON (String -> ListModelResponse) where
    parseJSON (Object o) = do
        ListModelResponse <$> (o .: "name")
                         <*> (o .: "meta")
                         <*> (o .: "size")
                         <*> (o .: "history")
                         <*> (o .: "num_pos")
                         <*> (o .: "num_neg")
                         <*> (o .: "start_time")
                         <*> (o .: "end_time")
    parseJSON _ = mzero

instance ToJSON ListModelResponse where
    toJSON (ListModelResponse name' meta' size' history' num_pos' num_neg' start_time' end_time' uuid')= 
        object ["name" .= name'
               , "meta" .= meta'
               , "size" .= size'
               , "history" .= history'
               , "num_pos" .= num_pos'
               , "num_neg" .= num_neg'
               , "start_time" .= start_time'
               , "end_time" .= end_time'
               , "uuid" .= uuid'
               , "image" .= ("models/" <> uuid' <> "/image.jpg")
               ]
        


