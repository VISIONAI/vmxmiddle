{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Handler.CheckLicense where

import Import
import Data.Aeson(decode)
import qualified Data.Text.IO as DT (readFile)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Text (unpack)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.List (last, head)
import Prelude (tail)
import Data.IORef (readIORef, writeIORef)





data VMXServerMessage = VMXServerMessage {
    message :: String,
    version :: Maybe String
}

instance FromJSON VMXServerMessage where
    parseJSON (Object o) = VMXServerMessage <$> (o .: "message") <*> (o .:? "version")
    parseJSON _ = mzero
    



getCheckLicenseR :: Handler Value
getCheckLicenseR = do
    vmxExecutable' <- vmxExecutable
    matlabRuntime' <- matlabPath
    (exitCode, stdout, _) <- liftIO $ readProcessWithExitCode  vmxExecutable' [matlabRuntime', "licenseCheckSlug"] ""
    case exitCode of
        ExitSuccess    -> return $ object ["licensed" .= True]
        ExitFailure 11 -> do
            let uuid = getUUID . readJson . last . lines $ stdout
            let version = getVersion $ readJson $ head $ lines stdout

            setMachineIdent uuid
            return $ object ["licensed" .= False, "uuid" .= uuid, "version" .= version]
        _  -> error "undefined exit code for vmxserver"
    where
        getVersion :: VMXServerMessage -> String
        getVersion s = fromMaybe "no version" $ version s
        
        
        getUUID :: VMXServerMessage -> String
        getUUID s =  reverse . takeWhile notSemi $ reverse . message $ s
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
                         undefined 

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
