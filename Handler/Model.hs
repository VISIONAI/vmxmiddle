{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Model where

import Import
import Helper.Shared
import Control.Monad (mzero)
import qualified          Data.String.Utils  as S (replace)
import           Data.List.Split (splitOn)
import qualified Graphics.GD.ByteString as G
import qualified          Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C
import           Data.Text as T (pack,unpack, replace, Text, append)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Aeson (encode, eitherDecode, (.:?), decode)
import           Data.Typeable
import           GHC.Generics
import           Data.Data
import           Helper.VMXTypes
import           System.Directory (getDirectoryContents)
import           Data.Maybe (fromJust)
import           System.IO.Unsafe (unsafePerformIO)
import           Debug.Trace

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

putModelR :: Handler String
putModelR =  do
    headers
    cmd :: SaveModelCommand <- requireJsonBody
    let sid = saveModelSid cmd
    let req = object ["command" .= save_model, "session_id" .= sid]
    response <- getPipeResponse req sid
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
postModelR :: Handler String
postModelR = do
    headers
    cmc <- parseJsonBody_
    let name = createModelName cmc
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
        fixPath :: String -> FilePath -> String
        fixPath s base = S.replace base "" s
        writeImage :: VMXImage -> FilePath -> IO ()
        writeImage s p = do
            let meat = (splitOn comma $ vmxIImage s) !! 1
            img <-liftIO $  G.loadJpegByteString $  B64.decodeLenient $ C.pack $ meat
            liftIO $ G.saveJpegFile 95  p img

        selectionsAndFiles :: [VMXImage] -> SessionId -> Int -> FilePath -> [(FilePath, VMXImage)]
        selectionsAndFiles (x:xs) sid' count wwwDir' = (wwwDir' ++ "sessions/" ++ sid' ++ "/image" ++ (show count) ++ ".jpg", x) : (selectionsAndFiles xs sid' (count + 1) wwwDir')
        selectionsAndFiles [] _ _ _ = []

        comma :: String
        comma = ","

list_models :: Handler String
list_models = do
    extra <- getExtra
    modelsDir  <- (++ "models/") <$> wwwDir
    modelFolders <- liftIO $ getDirectoryContents modelsDir
    let modelJsons = map (\x -> modelsDir ++ x) $ modelsFrom modelFolders
    response <- liftIO $ sequence $ fmap readFile $ modelJsons
    let (models  :: [String -> ListModelResponse]) = map makeJson response
    return $ LC.unpack $ encode $ object ["data" .= zipWith (\a b-> a b) models (modelsFrom' modelFolders)]
    where
        modelsFrom []       = []
        modelsFrom (".":r)  = modelsFrom r
        modelsFrom ("..":r) = modelsFrom r
        modelsFrom (".DS_Store":r) = modelsFrom r
        modelsFrom (x:r)    = (x ++ "/model.json") : modelsFrom r
        modelsFrom' []       = []
        modelsFrom' (".":r)  = modelsFrom' r
        modelsFrom' ("..":r) = modelsFrom' r
        modelsFrom' (x:r)    = x : modelsFrom' r
        makeJson :: String -> (String -> ListModelResponse)
        makeJson s = do
            -- String -> Char8 bystring
            let packed = C.pack s
            -- Char8 -> Lazy bytestring
            let chunked = L.fromChunks [packed]
            let eJ :: Either String (String -> ListModelResponse) = eitherDecode chunked
            case eJ of
                Right r -> r
                -- TODO .. properly handle errors
                Left e -> do
                          ListModelResponse  "error" e [] [] e "error"  e  "error"  "error" 0 0  "error"  "error" 

data ListModelResponse = ListModelResponse {
    listModelName :: String,
    listModelMeta :: String,
    listModelSize :: [Int],
    listModelHistory :: [String],
    listModelDataset :: String,
    listModelNetwork :: String,
    listModelCompiled :: String,
    listModelPos      :: String,
    listModelStats    :: String,
    listModelNumPos   :: Int,
    listModelNumNeg   :: Int,
    listModelStartTime :: String,
    listModelEndTiem   :: String,
    listModelUUID      :: String
}


instance FromJSON ListModelResponse where
    parseJSON (Object o) = do
        ListModelResponse <$> (o .: "name")
                         <*> (o .: "meta")
                         <*> (o .: "size")
                         <*> (o .: "history")
                         <*> (o .: "data_set")
                         <*> (o .: "network")
                         <*> (o .: "compiled")
                         <*> (o .: "pos")
                         <*> (o .: "stats")
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
                         <*> (o .: "data_set")
                         <*> (o .: "network")
                         <*> (o .: "compiled")
                         <*> (o .: "pos")
                         <*> (o .: "stats")
                         <*> (o .: "num_pos")
                         <*> (o .: "num_neg")
                         <*> (o .: "start_time")
                         <*> (o .: "end_time")
    parseJSON _ = mzero

instance ToJSON ListModelResponse where
    toJSON (ListModelResponse name meta size history dataset network compiled pos stats num_pos num_neg start_time end_time uuid)= 
        object ["name" .= name
               , "meta" .= meta
               , "size" .= size
               , "history" .= history
               , "dataset" .= dataset
               , "network" .= network
               , "compiled" .= compiled
               , "pos" .= pos
               , "stats" .= stats
               , "num_pos" .= num_pos
               , "num_neg" .= num_neg
               , "start_time" .= start_time
               , "end_time" .= end_time
               , "uuid" .= uuid
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

