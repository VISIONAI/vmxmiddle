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
import           Data.Typeable
import           GHC.Generics
import           Data.Data
--import           Helper.VMXTypes
import           System.Directory (getDirectoryContents,doesFileExist)
import qualified Data.Text.IO as DT (readFile)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.String.Utils (replace)

-- optionsModelR :: Handler ()
-- optionsModelR = do
--     addHeader "Allow" "Get"
--     addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
--     addHeader "Access-Control-Allow-Methods" "GET"
--     return ()


--list all models
getModelR :: Handler TypedContent
getModelR = do
    ret <- list_models
    selectRep $ do
        provideRepType  mimeJson $ return ret
        provideRepType  mimeHtml $ return $ ("<pre>" <> (decodeUtf8 $ encodePretty $ ret) <> "</pre>")
        provideRepType  mimeText $ return ret

-- data SaveModelCommand = SaveModelCommand {
--     saveModelSid :: String,
--     saveModelName :: String,
--     saveModelNewUUID :: Maybe Bool
                     
-- }

-- instance FromJSON SaveModelCommand where
--     parseJSON (Object o) =
--         SaveModelCommand <$> (o .: "session_id")
--         <*> (o .: "name")
--         <*> (o .:? "new_uuid")
--     parseJSON _ = mzero

-- save the model
-- putModelR :: Handler TypedContent
-- putModelR =  do
--     addHeader "Content-Type" "application/json"
--     cmd :: SaveModelCommand <- requireJsonBody
--     let sid = saveModelSid cmd
--     let name = saveModelName cmd
--     let new_uuid = saveModelNewUUID cmd

--     let req = object ["command" .= save_model, "name" .= name, "new_uuid" .= new_uuid]
--     _ <- getPortResponse req sid
--     ret <- getSessionInfo sid
--     selectRep $ do
--         provideRepType  mimeJson $ return ret
--         provideRepType  mimeHtml $ return ret
--         provideRepType  mimeText $ return ret

--     where
--         save_model :: String = "save_model"


list_models :: Handler Value
list_models = do
    modelsDir      <- (++ "models/") <$> wwwDir
    -- all folders in the models directory that don't start with a dot
    modelFolders   <- fmap (filter $ not . startsWithDot) $ liftIO $ getDirectoryContents modelsDir
    let modelJsons = map (\x -> modelsDir ++ x) $ jsonFilesFrom modelFolders
    --  only the model.jsons that actually exist
    modelJsons'    <- liftIO $  filterM doesFileExist modelJsons
    response       <- liftIO $ sequence $ fmap DT.readFile$ modelJsons'
    -- Now take the valid json file names, and strip them to get the valid UUIDs
    let validUUIDs = map (\x -> Data.String.Utils.replace "/model.json" "" x) $ map (\x -> Data.String.Utils.replace modelsDir "" x) modelJsons'
    let models     = map makeJson' (map unpack response)
    return $ object ["data" .= models] --zipWith (\a b-> a b) models (validUUIDs)]
    where
        jsonFilesFrom folders = map (++ "/model.json") folders
        startsWithDot (c:_)   = c == '.'
        startsWithDot _       = undefined
        makeJson' :: String -> (ListModelResponse)
        makeJson' s = do
            -- String -> Char8 bystring
            let packed = C.pack s
            -- Char8 -> Lazy bytestring
            let chunked = L.fromChunks [packed]
            let eJ :: Either String (ListModelResponse) = eitherDecode chunked
            case eJ of
                Right r -> r
                -- TODO .. properly handle errors
                Left e -> do
                          ListModelResponse e e e [] False e e [] 0 0 e e

data ListModelResponse = ListModelResponse {
    listModelName :: String,
    listModelUUID      :: String,
    listModelMeta :: String,
    listModelHistory :: [String],
    listModelCompiled :: Bool,
    listModelVersion   :: String,
    listModelImage :: String,
    listModelSize :: [Int],
    listModelNumPos   :: Int,
    listModelNumNeg   :: Int,
    listModelStartTime :: String,
    listModelEndTime   :: String
                          
}


instance FromJSON ListModelResponse where
    parseJSON (Object o) = do
        ListModelResponse <$> (o .: "name")
                         <*> (o .: "uuid")
                         <*> (o .: "meta")
                         <*> (o .: "history")
                         <*> (o .: "compiled")
                         <*> (o .: "version")
                         <*> (o .: "image")
                         <*> (o .: "size")
                         <*> (o .: "num_pos")
                         <*> (o .: "num_neg")
                         <*> (o .: "start_time")
                         <*> (o .: "end_time")
    parseJSON _ = mzero

-- instance FromJSON (String -> ListModelResponse) where
--     parseJSON (Object o) = do
--         ListModelResponse <$> (o .: "name")
--                          <*> (o .: "uuid")
--                          <*> (o .: "meta")
--                          <*> (o .: "history")
--                          <*> (o .: "compiled")
--                          <*> (o .: "version")
--                          <*> (o .: "image")
--                          <*> (o .: "size")
--                          <*> (o .: "num_pos")
--                          <*> (o .: "num_neg")
--                          <*> (o .: "start_time")
--                          <*> (o .: "end_time")
--     parseJSON _ = mzero


-- instance FromJSON (String -> ListModelResponse) where
--     parseJSON (Object o) = do
--         ListModelResponse <$> (o .: "name")
--                          <*> (o .: "meta")
--                          <*> (o .: "size")
--                          <*> (o .: "history")
--                          <*> (o .: "num_pos")
--                          <*> (o .: "num_neg")
--                          <*> (o .: "start_time")
--                          <*> (o .: "end_time")
--     parseJSON _ = mzero

instance ToJSON ListModelResponse where
    toJSON (ListModelResponse name' uuid' meta' history' compiled' version' image' size' num_pos' num_neg' start_time' end_time')= 
        object ["name" .= name'
               , "uuid" .= uuid'
               , "meta" .= meta'
               , "history" .= history'
               , "compiled" .= compiled'
               , "version" .= version'
               , "image" .= image'
               , "size" .= size'
               , "num_pos" .= num_pos'
               , "num_neg" .= num_neg'
               , "start_time" .= start_time'
               , "end_time" .= end_time'
               ]
        

-- data VMXModel = VMXModel {
--     name :: String,
--     hg_size :: [Int],
--     num_pos :: Int,
--     num_neg :: Int,
--     image ::   String,
--     start_time :: Float,
--     end_time :: Float
-- }  deriving (Data, Typeable, Show, Generic)

-- instance FromJSON VMXModel
-- instance ToJSON VMXModel

