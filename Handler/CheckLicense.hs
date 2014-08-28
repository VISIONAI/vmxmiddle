{-# LANGUAGE ScopedTypeVariables  #-}
module Handler.CheckLicense where

import Import
import Data.Aeson(decode)
import qualified Data.Text.IO as DT (readFile)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Text (unpack)



data VMXServerConfig = VMXServerConfig {
    license :: String,
    key     :: String
}

instance FromJSON VMXServerConfig where
    parseJSON (Object o) = VMXServerConfig <$> (o .: "license") <*> (o .: "user")
    parseJSON _ = mzero

--keyGenAlgo :: LicenseKey -> UUID -> Secret -> Text
--keyGenAlgo l u s = T.pack . showDigest . sha512 . toBL $ l <> u <> s

getCheckLicenseR :: Handler Value
getCheckLicenseR = do
    extra <-getExtra
    let path = (fromMaybe "/vmx/build" $ extraVmxPath extra) ++ "/VMXServerConfig.json"
    config <- liftIO $ readJson . unpack <$> DT.readFile path 
    return $ object ["license" .= license config]
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
                         undefined 
