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
                        --(exitCode, stdout, _) <- liftIO $ readProcessWithExitCode  vmxExecutable' ["-check"] "" 
                        return (exitCode, unpack stdout)
    let uuid = getUUID . readJson . List.head . lines $ stdout
    let version = getVersion $ readJson $ List.head $ lines stdout
    -- liftIO $ print $ show . head . lines $ stdout
    setMachineIdent uuid

    case exitCode of
        ExitSuccess    -> do
            liftIO $ DT.writeFile licensePath (pack . List.head . lines $ stdout)
            return $ object ["licensed" .= True, "uuid" .= uuid, "version" .= version]
        ExitFailure 11 -> do
            return $ object ["licensed" .= False, "uuid" .= uuid, "version" .= version]
        ExitFailure 127  -> error $ "Error 127: Cannot Find " <> show vmxExecutable'
        ExitFailure 126  -> error $ "Error 126: Cannot Start " <> show vmxExecutable' <> " message: " <> stdout
        ExitFailure 133  -> error $ "Error 33: Cannot Start " <> show vmxExecutable'
        ExitFailure x  -> error $ "Undefined exit code: " <> show x <> vmxExecutable' <> " message: " <> stdout
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
