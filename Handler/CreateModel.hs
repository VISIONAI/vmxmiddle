{-# LANGUAGE ScopedTypeVariables #-}
module Handler.CreateModel where

import Import
import Helper.Shared
import Helper.VMXTypes
--import Data.Aeson (decode', encode)
--import Data.Aeson.Types (Result(..))
--import qualified Data.ByteString.Lazy.Char8 as LBS
--import Network.HTTP.Types (status400)

optionsCreateModelR :: SessionId -> Handler ()
optionsCreateModelR _ = do
    addHeader "Allow" "POST"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "POST"
    return ()

data CreateModelCommand = CreateModelCommand {
    createModelName :: String,
    createModelParams :: Maybe VMXParams,
    createModelImages :: [VMXImage],
    createModelPretrained :: Maybe String
}

instance FromJSON CreateModelCommand where
    parseJSON (Object o) = do
        CreateModelCommand <$> (o .: "name") <*> (o .:? "params") <*> (o .: "images") <*> (o .:? "pretrained")
    parseJSON _ = mzero


--create new model
postCreateModelR :: SessionId -> Handler TypedContent
postCreateModelR sid = do
    addHeader "Content-Type" "application/json"
    (cmc :: CreateModelCommand) <- requireJsonBody
    
    wwwDir' <- wwwDir
    let saf = (selectionsAndFiles (createModelImages cmc) sid 1 wwwDir')
    --no longer writing images to disk, so this can be cleaned up quite a bit
    --liftIO $ sequence $ map (\(x, y) -> writeImage y x)  saf
    let images = map snd saf
    let params = fromMaybe (VMXParams Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (createModelParams cmc)
    let req = object ["name"       .= createModelName cmc,
                      "params"     .= params,
                      "images"     .= images,
                      "pretrained" .= fromMaybe "" (createModelPretrained cmc),
                      "command"    .= ("create" :: String)
                     ]
    ret2 <- getPortResponse req sid
    ret <- getSessionInfo sid
    selectRep $ do
        provideRepType  mimeJson $ return ret2
        provideRepType  mimeHtml $ return ret2
        provideRepType  mimeText $ return ret2


    where
        selectionsAndFiles :: [VMXImage] -> SessionId -> Int -> FilePath -> [(FilePath, VMXImage)]
        selectionsAndFiles (x:xs) sid' counter wwwDir' = (wwwDir' ++ "sessions/" ++ sid' ++ "/image" ++ (show counter) ++ ".jpg", x) : (selectionsAndFiles xs sid' (counter + 1) wwwDir')
        selectionsAndFiles [] _ _ _ = []


