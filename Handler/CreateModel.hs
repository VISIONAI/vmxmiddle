{-# LANGUAGE ScopedTypeVariables #-}
module Handler.CreateModel where

import Import
import Helper.Shared
import Helper.VMXTypes
--import Data.Aeson (decode', encode)
--import Data.Aeson.Types (Result(..))
--import qualified Data.ByteString.Lazy.Char8 as LBS
--import Network.HTTP.Types (status400)

data CreateModelCommand = CreateModelCommand {
    createModelName :: String,
    createModelParams :: Maybe VMXParams,
    createModelImages :: [VMXImage],
    createModelPretrained :: Maybe String
}

instance FromJSON CreateModelCommand where
    parseJSON (Object o) = do
        CreateModelCommand <$> (o .: "name")
                           <*> (o .:? "params")
                           <*> (o .: "images")
                           <*> (o .:? "pretrained")
    parseJSON _ = mzero

optionsCreateModelR :: SessionId -> Handler ()
optionsCreateModelR _ = do
    addHeader "Allow" "Post"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "POST"
    return ()

postCreateModelR :: SessionId -> Handler TypedContent
postCreateModelR sid = do
    (cmc :: CreateModelCommand) <- requireJsonBody
    let params = fromMaybe (VMXParams Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (createModelParams cmc)
    let req = object ["name"       .= createModelName cmc,
                      "params"     .= params,
                      "images"     .= createModelImages cmc,
                      "pretrained" .= fromMaybe "" (createModelPretrained cmc),
                      "command"    .= ("create" :: String)
                     ]
    ret2 <- getPortResponse req sid
    -- NOTE: this payload will not return the session ID
    --ret <- getSessionInfo sid
    selectRep $ do
        provideRepType  mimeJson $ return ret2
        provideRepType  mimeHtml $ return ret2
        provideRepType  mimeText $ return ret2


