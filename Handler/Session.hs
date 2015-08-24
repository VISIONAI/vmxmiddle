{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Session where

import Import
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import System.IO
import System.IO.Error (isAlreadyExistsError)
import System.Process
import System.Directory (createDirectory, doesFileExist)
import Data.UUID.V4 as U4 (nextRandom)
import Data.UUID as U (toString)
import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Exception (tryJust, catch)
import Data.Char

import qualified Data.Text.IO as DT (readFile)
import Data.List (isInfixOf)
import Data.Map (keys)
-- import qualified Data.ByteString.Lazy.Char8 as LC

import Control.Concurrent (threadDelay)
import Helper.Shared

optionsSessionR :: Handler ()
optionsSessionR = do
    addHeader "Allow" "Get, Post"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET, POST"
    return ()




postSessionR :: Handler TypedContent
postSessionR = do
    addHeader "Access-Control-Allow-Origin" "*"
    (csc :: CreateSessionCommand ) <- requireJsonBody
    response <- createSession (sessionID csc)
    selectRep $ do
      provideRepType  mimeJson $ return response
      provideRepType  mimeHtml $ return response
      provideRepType  mimeText $ return response


type ModelName = String

createSession :: Maybe String -> Handler Value
createSession msid = do
    sid <- case msid of
      Nothing -> do
        liftIO $ getSessionId
      Just s -> do
        return s

    let cleansid = filter good sid
    if (length sid == 0) || (not $ isInfixOf sid cleansid)
      then error $ "id is empty or contains invalid character (must be lowercase alphanumeric with dashes)"
      else liftIO $ print "No invalid characters here"


    App _ _ _ _ portMap' _ _ <- getYesod
    portMap <- do
        pm <- liftIO $ takeMVar portMap'
        liftIO $ putMVar portMap' pm
        return pm
               
    let sessions' = keys $ portMap
    if elem sid sessions'
       then do
         error $ "id already used up"
      else do
         liftIO $ print "New id is not used up"



        
    sessionPath'    <- sessionPath sid
    lift $ 
        createDirectory sessionPath' `catch`
        (\e -> if isAlreadyExistsError e 
               then liftIO $ print "Welcome"
               else liftIO $ print "Welcome2")
    
    outLogPath'     <- outLogPath sid
    vmxExecutable'  <- vmxExecutable
    port            <- addLock sid Nothing
    dataDir         <- wwwDir

    let shellLine = unwords [vmxExecutable', dataDir, sid, "none", ":" ++ show port]

    log'    <- lift $ openFile outLogPath' AppendMode
    (_,_,_,ph)      <- lift $ createProcess (shell $ shellLine)
                         {std_out = UseHandle log'}
    
    liftIO $ 
        waitForFile (sessionPath' ++ "/url") ph vmxExecutable'
    resid <- getSessionInfo sid
    return $ object ["data" .= resid]
    where
        -- we only allow session ids to contain alphanumeric caracters and dashes
        good x = (not $ isUpper x) && isAlphaNum x || x == '-'
        asString = C.unpack . C.concat . L.toChunks . encode
        waitForFile :: FilePath -> ProcessHandle -> String -> IO ()
        waitForFile f ph vmxExecutable' = do
            --liftIO $ print "Waiting..."
            ready <- doesFileExist f
            ec <- getProcessExitCode ph
            case ec of
              Nothing -> return () --liftIO $ print "good"
              Just e -> error $ "Error " ++ (show e) ++": Cannot Start " <> vmxExecutable'
            if ready
                then return ()
                else do
                    threadDelay 200
                    waitForFile f ph vmxExecutable'

        getSessionId :: IO String
        getSessionId = do
            seed <- U4.nextRandom
            return $ U.toString seed
        sessionPath :: SessionId -> Handler String
        sessionPath  sid = do
            dir <- wwwDir 
            return $ dir ++ "sessions/" ++ sid
        outLogPath :: SessionId -> Handler String
        outLogPath  sid = fmap (++ "/log.txt") (sessionPath sid) >>= return
        --modelPath = case modelNameM of
        --        Just m -> "models/" ++ m ++ ".mat"
        --        Nothing -> ""

--list all sessions
getSessionR :: Handler TypedContent
getSessionR = do
    addHeader "Access-Control-Allow-Origin" "*"
    ret <- list_sessions
    selectRep $ do
        provideRepType  mimeJson $ return ret
        provideRepType  mimeHtml $ return $ ("<pre>" <> (decodeUtf8 $ encodePretty $ ret) <> "</pre>")
        provideRepType  mimeText $ return ret


list_sessions :: Handler Value
list_sessions = do
    App _ _ _ _ portMap' _ _ <- getYesod
    portMap <- do
        pm <- liftIO $ takeMVar portMap'
        liftIO $ putMVar portMap' pm
        return pm
               
    -- liftIO $ print $ "keys are" ++ (show $ keys $ portMap)

    -- sessions <-  sp >>= lift.getDirectoryContents 
    -- let sessions'' = filter notDots sessions
    -- psOutput <- liftIO $ readProcess "ps" ["aux"] ""
    -- let sessions' = filter (notDead psOutput) sessions''
    -- liftIO $ print $ "sessions' is " ++ (show sessions')
    
    let sessions' = keys $ portMap
    --liftIO $ print $ "portMap' is " ++ (show keys portMap')
    out <- sequence $ map getSessionInfo sessions'
    return $ object ["data" .= out]
          
-- create a new session
data VmxSessionFile = VmxSessionFile {
        pid :: String
} 



instance FromJSON VmxSessionFile where
    parseJSON (Object o) = VmxSessionFile <$> (o .: "pid")
    parseJSON _ = mzero


-- create a new session
data CreateSessionCommand = CreateSessionCommand {
--        modelUUIDS :: [String],
        sessionID :: Maybe String
} 



instance FromJSON CreateSessionCommand where
    parseJSON (Object o) = CreateSessionCommand <$> (o .:? "id")
    parseJSON _ = mzero


