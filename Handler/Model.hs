{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Aeson (eitherDecode, (.:?))
import           Data.Typeable
import           GHC.Generics
import           Data.Data


optionsModelR :: Handler ()
optionsModelR = do
    addHeader "Allow" "Get, Put, Post"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET, PUT, POST"
    return ()


--list all models
getModelR :: Handler Value
getModelR = do
    addHeader "Access-Control-Allow-Origin" "*"
    liftIO $ list_models >>= return

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
    createModelCls :: String,
    createModelParams :: Value,
    createModelSelections :: [CreateModelSelection],
    createModelSid :: String
}

instance FromJSON CreateModelCommand where
    parseJSON (Object o) = do
        CreateModelCommand <$> (o .: "cls") <*> (o .: "params") <*> (o .: "selections") <*> (o .: "session_id")
    parseJSON _ = mzero


data CreateModelSelection = CreateModelSelection {
    selectionBB :: [Float],
    selectionIcon :: String,
    selectionImage :: String,
    selectionTime :: Int
}

instance FromJSON CreateModelSelection where
    parseJSON (Object o) = do
        CreateModelSelection <$> (o .: "bb") <*> (o .: "icon") <*> (o .: "image") <*> (o .: "time")
    parseJSON _ = mzero


instance ToJSON CreateModelSelection where
    toJSON (CreateModelSelection bb icon image time) =
            object ["bb" .= bb, "icon" .= icon, "image" .= image, "time" .= time]


--create new model
postModelR :: Handler String
postModelR = do
    headers
    cmc <- parseJsonBody_
    let sid = createModelSid cmc
    wwwDir' <- wwwDir
    let saf = (selectionsAndFiles (createModelSelections cmc) sid 1 wwwDir')
    liftIO $ sequence $ map (\(x, y) -> writeImage y x)  saf
    let mlSelections = map (\(path, selection) -> object ["bb" .= selectionBB selection
                                                         ,"icon" .= selectionIcon selection
                                                         , "image" .= fixPath path wwwDir'
                                                         , "time" .= selectionTime selection
                                                         ]) saf
    let req = object ["cls"        .= createModelCls cmc,
                      "params"     .= createModelParams cmc,
                      "selections" .= mlSelections,
                      "session_id" .= sid,
                      "command"    .= ("create_model" :: String)
                     ]
    response <- getPipeResponse req sid
    return response
    where
        fixPath :: String -> FilePath -> String
        fixPath s base = S.replace base "" s
        writeImage :: CreateModelSelection -> FilePath -> IO ()
        writeImage s p = do
            let meat = (splitOn comma $ selectionImage s) !! 1
            img <-liftIO $  G.loadJpegByteString $  B64.decodeLenient $ C.pack $ meat
            liftIO $ print $ "path is " ++ p
            liftIO $ G.saveJpegFile 95  p img

        selectionsAndFiles :: [CreateModelSelection] -> SessionId -> Int -> FilePath -> [(FilePath, CreateModelSelection)]
        selectionsAndFiles (x:xs) sid' count wwwDir' = (wwwDir' ++ "sessions/" ++ sid' ++ "/image" ++ (show count) ++ ".jpg", x) : (selectionsAndFiles xs sid' (count + 1) wwwDir')
        selectionsAndFiles [] _ _ _ = []

        comma :: String
        comma = ","

list_models :: IO Value
list_models = do
    models <- readProcess "sh" ["-c","ls /www/vmx/models/*mat 2> /dev/null || true"]  ""
    response <- sequence $ fmap readFile $ getJsonFiles' models
    response' <- sequence $  fmap makeJson response
    return $ object ["data" .= response']
    where
        modelsPath base = base ++ "models/*.mat"
        makeJson :: String -> IO VMXModel
        makeJson s = do
            -- String -> Char8 bystring
            let packed = C.pack s
            -- Char8 -> Lazy bytestring
            let chunked = L.fromChunks [packed]
            let eJ :: Either String VMXModel = eitherDecode chunked
            case eJ of
                Right r -> return r
                -- TODO .. properly handle errors
                Left e -> do
                          liftIO $ print "slapper dodole"
                          return undefined
        getJsonFiles' :: String -> [FilePath]
        getJsonFiles' m' = lines $ T.unpack $ T.replace ".mat" ".json" $ T.replace "models/" "models/summary/" $ T.pack m'

data VMXModel = VMXModel {
    cls :: String,
    hg_size :: [Int],
    num_pos :: Int,
    num_neg :: Int,
    image ::   String,
    start_time :: Float,
    end_time :: Float
}  deriving (Data, Typeable, Show, Generic)

instance FromJSON VMXModel
instance ToJSON VMXModel

