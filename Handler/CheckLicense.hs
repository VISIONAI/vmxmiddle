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
import Data.List (last)
import Prelude (tail)
import Data.IORef (readIORef, writeIORef)





data VMXServerMessage = VMXServerMessage {
    message :: String
}

instance FromJSON VMXServerMessage where
    parseJSON (Object o) = VMXServerMessage <$> (o .: "message")
    parseJSON _ = mzero
    



getCheckLicenseR :: Handler Value
getCheckLicenseR = do
    vmxExecutable' <- vmxExecutable
    matlabRuntime' <- matlabPath
    (exitCode, stdout, _) <- liftIO $ readProcessWithExitCode  vmxExecutable' [matlabRuntime', "licenseCheckSlug"] ""
    case exitCode of
        ExitSuccess    -> return $ object ["licensed" .= True]
        ExitFailure 11 -> do
            let uuid = getUUID $ readJson $ last $ lines stdout
            setMachineIdent uuid
            return $ object ["licensed" .= False, "uuid" .= uuid]
        _  -> error "undefined exit code for vmxserver"
    where
        getUUID :: VMXServerMessage -> String
        getUUID s = tail' $ dropWhile notSemi $ dropWhile notSemi $ message s
            where
                notSemi :: (Char -> Bool)
                notSemi = not . (== ';')
                -- safe version of Prelude.tail
                tail' [] = []
                tail'  a  = tail a
        
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
