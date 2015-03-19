{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Handler.CheckLicense where

import Import
import qualified Data.Text.IO as DT (readFile,writeFile)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import System.Exit (ExitCode(..))
import qualified Data.List as List (head)
import Prelude (tail)
import Data.IORef (readIORef, writeIORef)
import System.Directory (doesFileExist)
import Data.Text.IO (hGetContents)

import System.Process
import Control.Exception (tryJust)
import System.IO.Error (isDoesNotExistError)
import Control.Monad (guard)


data VMXServerMessage = VMXServerMessage {
    message :: String,
    version :: String,
    machine :: String,
    user :: String
}

instance FromJSON VMXServerMessage where
    parseJSON (Object o) = VMXServerMessage <$> (o .: "message")
                           <*> (o .: "version")
                           <*> (o .: "machine")
                           <*> (o .: "user")
    parseJSON _ = mzero
    



getCheckLicenseR :: Handler Value
getCheckLicenseR = do
    addHeader "Access-Control-Allow-Origin" "*"
    extra <-getExtra
    let vmxPath     = (fromMaybe "/vmx/build" $ extraVmxPath extra)
        licensePath = vmxPath ++ "/.vmxlicense"
    licensed <- liftIO . doesFileExist $ licensePath
        
    vmxExecutable' <- vmxExecutable


    (exitCode, stdout) <-
                case licensed of 
                    True -> do
                        stdout <- liftIO $ DT.readFile licensePath
                        return (ExitSuccess, unpack stdout)
                    False -> do 
                        (_, Just stdoutHdl, _, hdl) <- liftIO $ createProcess (proc vmxExecutable' ["-check"]) {std_out =CreatePipe}
                        stdout <- liftIO $ Data.Text.IO.hGetContents stdoutHdl 
                        exitCode <- liftIO $ waitForProcess hdl
                        return (exitCode, unpack stdout)
    let obj = readJson $ safeHead $ lines stdout
    let uuid = getUUID $ obj
    let version = getVersion $ obj
    -- liftIO $ print $ show . head . lines $ stdout
    setMachineIdent uuid
    
    e <- liftIO $ tryJust (guard . isDoesNotExistError) (readFile "version")
    let versionMiddle = either (const "development") id e

    case exitCode of
        ExitSuccess    -> do
            liftIO $ DT.writeFile licensePath (pack . safeHead . lines $ stdout)
            return $ object ["licensed" .= True, "uuid" .= uuid, "version" .= [version, versionMiddle]]
        ExitFailure 11 -> do
            return $ object ["licensed" .= False, "uuid" .= uuid, "version" .= [version, versionMiddle]]
        ExitFailure 127  -> error $ "Error 127: Cannot Find " <> show vmxExecutable' <> " message: " <> stdout
        ExitFailure 126  -> error $ "Error 126: Cannot Start " <> show vmxExecutable' <> " message: " <> stdout
        ExitFailure 133  -> error $ "Error 133: Cannot Start " <> show vmxExecutable' <> " message: " <> stdout
        ExitFailure x  -> error $ "Error " <> show x <> ": Cannot Start " <> vmxExecutable' <> " message: " <> stdout
    where

        safeHead :: [String] -> String
        safeHead (x:_) = x
        safeHead [] = ""

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
                Left e -> do
                         error $ "Invalid .vmxlicense file, please delete it and try again."

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
