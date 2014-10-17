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
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Aeson (encode)
import           Data.Typeable
import           GHC.Generics
import           Data.Data
import           Helper.VMXTypes
import           System.Directory (getDirectoryContents,doesFileExist)
import qualified Data.Text.IO as DT (readFile)

optionsModelR :: Handler ()
optionsModelR = do
    addHeader "Allow" "Get, Put, Post"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET, PUT, POST"
    return ()


--list all models
getModelR :: Handler String
getModelR = do
    addHeader "Access-Control-Allow-Origin" "*"
    list_models >>= return

data SaveModelCommand = SaveModelCommand {
    saveModelSid :: String
}

instance FromJSON SaveModelCommand where
    parseJSON (Object o) =
        SaveModelCommand <$> (o .: "session_id")
    parseJSON _ = mzero

putModelR :: Handler TypedContent
putModelR =  do
    headers
    cmd :: SaveModelCommand <- requireJsonBody
    let sid = saveModelSid cmd
    let req = object ["command" .= save_model, "session_id" .= sid]
    response <- getPortResponse req sid
    return response
    where
        save_model :: String = "save_model"

data CreateModelCommand = CreateModelCommand {
    createModelName :: String,
    createModelParams :: Value,
    createModelImages :: [VMXImage],
    createModelSid :: String
}

instance FromJSON CreateModelCommand where
    parseJSON (Object o) = do
        CreateModelCommand <$> (o .: "name") <*> (o .: "params") <*> (o .: "images") <*> (o .: "session_id")
    parseJSON _ = mzero





--create new model
postModelR :: Handler TypedContent
postModelR = do
    headers
    cmc <- requireJsonBody
    let sid = createModelSid cmc
    wwwDir' <- wwwDir
    let saf = (selectionsAndFiles (createModelImages cmc) sid 1 wwwDir')
    --no longer writing images to disk, so this can be cleaned up quite a bit
    --liftIO $ sequence $ map (\(x, y) -> writeImage y x)  saf
    let images = map snd saf
    let req = object ["name"       .= createModelName cmc,
                      "params"     .= createModelParams cmc,
                      "images"     .= images,
                      "command"    .= ("create_model" :: String)
                     ]
    response <- getPipeResponse req sid
    return response
    where
        selectionsAndFiles :: [VMXImage] -> SessionId -> Int -> FilePath -> [(FilePath, VMXImage)]
        selectionsAndFiles (x:xs) sid' counter wwwDir' = (wwwDir' ++ "sessions/" ++ sid' ++ "/image" ++ (show counter) ++ ".jpg", x) : (selectionsAndFiles xs sid' (counter + 1) wwwDir')
        selectionsAndFiles [] _ _ _ = []

list_models :: Handler String
list_models = do
    modelsDir      <- (++ "models/") <$> wwwDir
    -- all folders in the models directory that don't start with a dot
    modelFolders   <- fmap (filter $ not . startsWithDot) $ liftIO $ getDirectoryContents modelsDir
    let modelJsons = map (\x -> modelsDir ++ x) $ jsonFilesFrom modelFolders
    --  only the model.jsons that actually exist
    modelJsons'    <- liftIO $  filterM doesFileExist modelJsons
    response       <- liftIO $ sequence $ fmap DT.readFile$ modelJsons'
    let models     = map makeJson' (map unpack response)
    return $ LC.unpack $ encode $ object ["data" .= zipWith (\a b-> a b) models (modelFolders)]
    where
        jsonFilesFrom folders = map (++ "/model.json") folders
        startsWithDot (c:_)   = c == '.'
        startsWithDot _       = undefined
        makeJson' :: String -> (String -> ListModelResponse)
        makeJson' s = do
            -- String -> Char8 bystring
            let packed = C.pack s
            -- Char8 -> Lazy bytestring
            let chunked = L.fromChunks [packed]
            let eJ :: Either String (String -> ListModelResponse) = eitherDecode chunked
            case eJ of
                Right r -> r
                -- TODO .. properly handle errors
                Left e -> do
                          ListModelResponse  e e [] [] 0 0  "error" "error"

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
        

data VMXModel = VMXModel {
    name :: String,
    hg_size :: [Int],
    num_pos :: Int,
    num_neg :: Int,
    image ::   String,
    start_time :: Float,
    end_time :: Float
}  deriving (Data, Typeable, Show, Generic)

instance FromJSON VMXModel
instance ToJSON VMXModel

