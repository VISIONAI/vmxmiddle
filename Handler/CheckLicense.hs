{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Handler.CheckLicense where

import Import
import Data.Aeson(decode)
import qualified Data.Text.IO as DT (readFile,writeFile)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Text (unpack)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.List (last, head)
import Prelude (tail)
import Data.IORef (readIORef, writeIORef)
import System.Directory (doesFileExist)





data VMXServerMessage = VMXServerMessage {
    message :: String,
    version :: String,
    machine :: String,
    user :: String
}

instance FromJSON VMXServerMessage where
    parseJSON (Object o) = VMXServerMessage <$> (o .: "message") <*> (o .: "version") <*> (o .: "machine") <*> (o .: "user")
    parseJSON _ = mzero
    



getCheckLicenseR :: Handler Value
getCheckLicenseR = do
    extra <-getExtra
    let path = (fromMaybe "/vmx/build" $ extraVmxPath extra) ++ "/.vmxlicense"
    licensed <- liftIO $ doesFileExist path
        
    vmxExecutable' <- vmxExecutable
    matlabRuntime' <- matlabPath
    (exitCode, stdout) <-
                case licensed of 
                    True -> do
                        stdout <- liftIO $ DT.readFile path
                        return (ExitSuccess, unpack stdout)
                    False -> do 
                        (exitCode, stdout, _) <- liftIO $ readProcessWithExitCode  
                                    vmxExecutable' [matlabRuntime', "licenseCheckSlug"] "" 
                        liftIO $ DT.writeFile path (pack . head . lines $ stdout)
                        return (exitCode, stdout)
    let uuid = getUUID . readJson . head . lines $ stdout
    let version = getVersion $ readJson $ head $ lines stdout
    -- liftIO $ print $ show . head . lines $ stdout
    setMachineIdent uuid

    case exitCode of
        ExitSuccess    -> return $ object ["licensed" .= True, "uuid" .= uuid, "version" .= version]
        ExitFailure 11 -> do
            return $ object ["licensed" .= False, "uuid" .= uuid, "version" .= version]
        ExitFailure 127  -> error $ "Error 127: Cannot Find " <> show vmxExecutable'
        ExitFailure 126  -> error $ "Error 126: Cannot Start " <> show vmxExecutable'
        ExitFailure 133  -> error $ "Error 126: Cannot Start " <> show vmxExecutable'
        ExitFailure x  -> error $ "Undefined exit code " <> show x
    where
        getVersion :: VMXServerMessage -> String
        getVersion s = version s
        
        
        getUUID :: VMXServerMessage -> String
        getUUID s =  reverse . takeWhile notSemi $ reverse . machine $ s
            where
                notSemi :: (Char -> Bool)
                notSemi = not . (== ';')
        
        readJson :: String -> VMXServerMessage
        readJson s = do
            let packed = C.pack s
            let chunked = L.fromChunks [packed]
            let eJ :: Either String VMXServerMessage = eitherDecode chunked
            case eJ of
                Right r -> r
                -- TODO .. properly handle errors
                Left e -> do
                         VMXServerMessage e e e e

setMachineIdent :: String -> Handler ()
setMachineIdent  ident = do
    App {..}   <- getYesod
    liftIO $ writeIORef machineIdent (Just ident)

getMachineIdent :: Handler (Maybe String)
getMachineIdent = do
    App {..}   <- getYesod
    ident' <- liftIO $ readIORef machineIdent
    return ident' 
    --extra <-getExtra
    --let path = (fromMaybe "/vmx/build" $ extraVmxPath extra) ++ "/VMXServerConfig.json"
    --config <- liftIO $ readJson . unpack <$> DT.readFile path 
