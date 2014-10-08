{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Handler.ActivateLicense where

import Import
import Data.Conduit (($$+-))
import Data.Conduit.Attoparsec  (sinkParser)
import Data.Aeson.Parser (json)
import Data.Aeson        (Result (..), fromJSON )
import Network.HTTP.Conduit (http, method, withManager, parseUrl, Response (..), HttpException (..) )
import Network.HTTP.Types (Status (..) )
import Handler.CheckLicense
import qualified Data.Text.Lazy.IO as DTL (writeFile)
import qualified Data.Text.IO as DT (readFile)
import qualified Data.ByteString.Char8 as C (pack)
import qualified Data.ByteString.Lazy as L (fromChunks)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Exception as X hiding (Handler)
import Data.Aeson.Encode.Pretty (encodePretty)

data ActivateResponse = ActivateResponse String

instance FromJSON ActivateResponse  where
    parseJSON (Object o) = ActivateResponse <$> (o .: "license")
    parseJSON _ = mzero

instance FromJSON VMXServerConfig  where
    parseJSON (Object o) = VMXServerConfig  <$> (o .: "user")
                                            <*> (o .: "license")
                                            <*> (o .: "vmx_dir")
                                            <*> (o .: "log_images")
                                            <*> (o .: "log_memory")
                                            <*> (o .: "display_images")
                                            <*> (o .: "MCR")
                                            <*> (o .: "eval_dir")
                                            <*> (o .: "pretrained")
    parseJSON _ = mzero

instance ToJSON VMXServerConfig where
    toJSON (VMXServerConfig user' license' vmx_dir' log_images' log_memory' display_images' mcr' evaldir' pretrained') =
        object ["user" .= user', "license" .= license', "vmx_dir" .= vmx_dir', "log_images" .= log_images', "log_memory" .= log_memory', "display_images" .= display_images', "MCR" .= mcr', "eval_dir" .= evaldir', "pretrained" .= pretrained']

data VMXServerConfig = VMXServerConfig {
    user            :: String,
    license         :: String,
    vmx_dir         :: String,
    log_images      :: Bool,
    log_memory      :: Bool,
    display_images  :: Bool,
    mcr             :: String,
    eval_dir        :: String,
    pretrained      :: String
}

writeLicense :: License -> LicenseKey -> Handler ()
writeLicense l key = do
    extra <-getExtra
    let path = (fromMaybe "/vmx/build" $ extraVmxPath extra) ++ "/config.json"
    c' <- liftIO $ readJson . unpack <$> DT.readFile path 
    case c' of
        VMXServerConfig _ _ vmx_dir' log_images' log_memory' display_images' mcr' vmxdata' pretrained' -> 
            liftIO $ DTL.writeFile path $ decodeUtf8 $ encodePretty $
                VMXServerConfig key l vmx_dir' log_images' log_memory' display_images' mcr' vmxdata' pretrained'
    where
        readJson :: String -> VMXServerConfig
        readJson s = do
            let packed = C.pack s
            let chunked = L.fromChunks [packed]
            let eJ :: Either String VMXServerConfig = eitherDecode chunked
            case eJ of
                Right r -> r
                -- TODO .. properly handle errors
                Left e -> do
                         VMXServerConfig e e "" False False False "" e e

postActivateLicenseR :: LicenseKey -> Handler Value
postActivateLicenseR key = do
    addHeader "Access-Control-Allow-Origin" "*"
    ident' <- getMachineIdent
    val <- case ident' of 
        Nothing -> error "no ident"
        Just uuid -> do 
            liftIO $ handle catchException $
                withManager $ \manager -> do
                    req' <- liftIO $ parseUrl $ "https://beta.vision.ai/license/" <> key <> "/file/" <> uuid
                    let req = req' { method = "POST"}
                    res <- (http req manager) 
                    resValue <- responseBody res $$+- sinkParser json
                    return resValue 

    _ <- case fromJSON val of
        Success (ActivateResponse licenseFile) -> do
            writeLicense licenseFile key
            return val
        Error s ->
            return $ object ["error" .= s]
    return val
    where
        catchException (StatusCodeException (Status 403 _) _ _) = return $ object ["error" .= ("key_already_used" :: String)]
        catchException (StatusCodeException (Status 404 _) _ _) = return $ object ["error" .= ("key_unknown" :: String)]
        catchException (StatusCodeException _ _ _) = return $ object ["error" .= ("unknown_error" :: String)]
        catchException _                           = return $ object ["error" .= ("non StatusCode unknown error" :: String)]
    



