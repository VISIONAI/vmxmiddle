{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Handler.ActivateLicense where

import Import
import Data.Conduit (($$+-))
import Data.Conduit.Attoparsec  (sinkParser)
import Data.Aeson.Parser (json)
import Data.Aeson        (Result (..), fromJSON , encode)
import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit (http, method, withManagerSettings, mkManagerSettings, parseUrl, Response (..), HttpException (..) , RequestBody (..), requestBody)
import Network.HTTP.Types (Status (..) )
import Handler.CheckLicense
import qualified Data.Text.Lazy.IO as DTL (writeFile)
import qualified Data.Text.IO as DT (readFile)
import qualified Data.ByteString.Char8 as C (pack)
import qualified Data.ByteString.Lazy as L (fromChunks)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Exception as X hiding (Handler)
import Data.Aeson.Encode.Pretty (encodePretty)
import System.Directory (doesDirectoryExist)

data ActivateResponse = ActivateResponse String

instance FromJSON ActivateResponse  where
    parseJSON (Object o) = ActivateResponse <$> (o .: "license")
    parseJSON _ = mzero

instance FromJSON VMXServerConfig  where
    parseJSON (Object o) = VMXServerConfig  <$> (o .: "user")
                                            <*> (o .: "license")
                                            <*> (o .: "models")
                                            <*> (o .: "sessions")
                                            <*> (o .: "log_images")
                                            <*> (o .: "log_memory")
                                            <*> (o .: "display_images")
                                            <*> (o .: "perform_tests")
                                            <*> (o .: "MCR")
                                            <*> (o .: "data")
                                            <*> (o .: "pretrained")
    parseJSON _ = mzero

instance ToJSON VMXServerConfig where
    toJSON (VMXServerConfig user' license' models' sessions' log_images' log_memory' display_images' perform_tests' mcr' vmxdata' pretrained') =
        object ["user" .= user', "license" .= license', "models" .= models', "sessions" .= sessions', "log_images" .= log_images', "log_memory" .= log_memory', "display_images" .= display_images', "perform_tests" .= perform_tests', "MCR" .= mcr', "data" .= vmxdata', "pretrained" .= pretrained']

data VMXServerConfig = VMXServerConfig {
    user            :: String,
    license         :: String,
    models          :: String,
    sessions        :: String,
    log_images      :: Bool,
    log_memory      :: Bool,
    display_images  :: Bool,
    perform_tests   :: Bool,
    mcr             :: String,
    vmxdata         :: String,
    pretrained      :: String
}

writeLicense :: License -> LicenseKey -> Handler ()
writeLicense l key = do
    extra <-getExtra
    let path = (fromMaybe "/vmx/build" $ extraVmxPath extra) ++ "/config.json"
    c' <- liftIO $ readJson . unpack <$> DT.readFile path 
    case c' of
        VMXServerConfig _ _ models' sessions' log_images' log_memory' display_images' perform_tests' mcr' vmxdata' pretrained' -> do
            liftIO $ DTL.writeFile path $ decodeUtf8 $ encodePretty $
                VMXServerConfig key l models' sessions' log_images' log_memory' display_images' perform_tests' mcr' vmxdata' pretrained'
            isDocker <- liftIO $ doesDirectoryExist dockerPersistDir
            if isDocker 
                then
                    liftIO $ DTL.writeFile (dockerPersistDir ++ "/config.json") $ decodeUtf8 $ encodePretty $
                        VMXServerConfig key l models' sessions' log_images' log_memory' display_images' perform_tests' mcr' vmxdata' pretrained'
                else return ()
             
                
    where
        dockerPersistDir = "/vmx-persist"
        readJson :: String -> VMXServerConfig
        readJson s = do
            let packed = C.pack s
            let chunked = L.fromChunks [packed]
            let eJ :: Either String VMXServerConfig = eitherDecode chunked
            case eJ of
                Right r -> r
                -- TODO .. properly handle errors
                Left e -> do
                         VMXServerConfig e e "" "" False False False False "" e e

data ActivatePayload =  ActivatePayload {
    activationPayloadEmail   :: Maybe Text
}

instance FromJSON ActivatePayload where
    parseJSON (Object o) = ActivatePayload <$> (o .:? "email")
    parseJSON _ = mzero

postActivateLicenseR :: LicenseKey -> Handler Value
postActivateLicenseR key = do
    incoming <- requireJsonBody
    ident' <- getMachineIdent
    val <- case ident' of 
        Nothing -> error "no ident"
        Just uuid -> do 
            let settings' = mkManagerSettings (TLSSettingsSimple True False False) Nothing
            liftIO $ handle catchException $
                withManagerSettings settings' $ \manager -> do
                    req' <- liftIO $ parseUrl $ "https://beta.vision.ai/license/" <> key <> "/file/" <> uuid
                    let valueBs = encode $ object ["email" .= activationPayloadEmail incoming]
                    let req = req' { method = "POST", requestBody = RequestBodyLBS valueBs}
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
        catchException x                           = do
            liftIO $ print x
            return $ object ["error" .= (show x)]
    



