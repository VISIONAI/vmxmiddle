{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-|
Module      : Check License
Description : VMX License Checking

This module contains functions for checking if VMX is licensed.
-}
module Handler.CheckLicense where

import Import
import qualified Data.Text.IO as DT (readFile,writeFile)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import System.Exit (ExitCode(..))
import qualified Data.List as List (head)
import Prelude (tail)
import Data.IORef (writeIORef)
import System.Directory (doesFileExist)
import Data.Text.IO (hGetContents)
import System.Process

import Control.Exception (tryJust)
import System.IO.Error (isDoesNotExistError)
import Control.Monad (guard)


{-|
The VMXServerMessage represents the first line of running VMXserver
-}
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
    
{-|
GET \/check

Handler for checking if the license is valid.

If a .vmxlicense file exists in the vmxPath, it will return true.
If the file is not present, it will run "VMXserver -check".  If the check
succeeds, it will write the .vmxlicense file and return true.  If the check
fails, it will return an error.

This will also set the machine identifier (the UUID)
-}
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
    
    e <- liftIO $ tryJust (guard . isDoesNotExistError) (readFile "version")
    let versionMiddle = either (const "development") id e

    case exitCode of
        ExitSuccess    -> do
            liftIO $ DT.writeFile licensePath (pack . List.head . lines $ stdout)
            return $ object ["licensed" .= True, "uuid" .= uuid, "version" .= [version, versionMiddle]]
        ExitFailure 11 -> do
            return $ object ["licensed" .= False, "uuid" .= uuid, "version" .= [version, versionMiddle]]
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

{-|
Sets the current computer's identifier.

See the related 'getMachineIdent' function.
-}
setMachineIdent :: String -> Handler ()
setMachineIdent  ident = do
    App {..}   <- getYesod
    liftIO $ writeIORef machineIdent (Just ident)

