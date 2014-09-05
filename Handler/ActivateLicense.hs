{-# LANGUAGE ScopedTypeVariables  #-}
module Handler.ActivateLicense where

import Import
import Data.Conduit (($$+-))
import Data.Conduit.Attoparsec  (sinkParser)
import Data.Aeson.Parser (json)
import Data.Aeson        (decode, encode, Result (..), fromJSON )
import Network.HTTP.Conduit (http, method, withManager, parseUrl, Response (..))
import Handler.CheckLicense
import qualified Data.Text.Lazy.IO as DTL (writeFile)
import qualified Data.Text.IO as DT (readFile)
import qualified Data.ByteString.Char8 as C (pack)
import qualified Data.ByteString.Lazy as L (fromChunks)
import Data.Text.Lazy.Encoding (decodeASCII)
import Data.Maybe (fromJust)

data ActivateResponse = ActivateResponse String

instance FromJSON ActivateResponse  where
    parseJSON (Object o) = ActivateResponse <$> (o .: "license")
    parseJSON _ = mzero

instance FromJSON VMXServerConfig  where
    parseJSON (Object o) = VMXServerConfig  <$> (o .: "user")
                                            <*> (o .: "license")
                                            <*> (o .: "models")
                                            <*> (o .: "sessions")
                                            <*> (o .: "apps")
                                            <*> (o .: "log_images")
                                            <*> (o .: "perform_tests")
                                            <*> (o .: "MCR")
                                            <*> (o .: "data")
                                            <*> (o .: "pretrained")
    parseJSON _ = mzero

instance ToJSON VMXServerConfig where
    toJSON (VMXServerConfig user license models sessions apps log_images perform_tests mcr vmxdata pretrained) =
        object ["user" .= user, "license" .= license, "models" .= models, "sessions" .= sessions, "apps" .= apps, "log_images" .= log_images, "perform_tests" .= perform_tests, "MCR" .= mcr, "data" .= vmxdata, "pretrained" .= pretrained]

data VMXServerConfig = VMXServerConfig {
    user            :: String,
    license         :: String,
    models          :: [String],
    sessions        :: [String],
    apps            :: [String],
    log_images      :: Bool,
    perform_tests   :: Bool,
    mcr             :: [String],
    vmxdata         :: String,
    pretrained      :: String
}

writeLicense :: License -> LicenseKey -> Handler ()
writeLicense l key = do
    extra <-getExtra
    let path = (fromMaybe "/vmx/build" $ extraVmxPath extra) ++ "/VMXServerConfig.json"
    c' <- liftIO $ readJson . unpack <$> DT.readFile path 
    case c' of
        VMXServerConfig _ _ models sessions apps log_images perform_tests mcr vmxdata pretrained -> 
            liftIO $ DTL.writeFile path $ decodeASCII $ encode $
                VMXServerConfig key l models sessions apps log_images perform_tests mcr vmxdata pretrained
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
                         VMXServerConfig e e [] [] [] False False [] e e

postActivateLicenseR :: LicenseKey -> Handler Value
postActivateLicenseR key = do
    ident' <- getMachineIdent
    val <- case ident' of 
        Nothing -> error "no ident"
        Just uuid -> do 
            liftIO $ withManager $ \manager -> do
                    req' <- liftIO $ parseUrl $ "https://beta.vision.ai/license/" <> key <> "/file/" <> uuid
                    let req = req' { method = "POST"}
                    res <- http req manager
                    resValue <- responseBody res $$+- sinkParser json
                    return resValue
    case fromJSON val of
        Success (ActivateResponse license) -> do
            liftIO $ print $ "license is " <> license
            writeLicense license key
            return val
        Error s ->
            error s
    return val
    



