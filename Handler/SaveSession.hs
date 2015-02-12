{-# LANGUAGE ScopedTypeVariables #-}

module Handler.SaveSession where

import Import
import Helper.Shared
import Data.Aeson.Types

-- import Data.Conduit
-- import Data.Conduit.List (consume)
-- import qualified Data.ByteString as S

data SaveSessionCommand = SaveSessionCommand {
    saveSessionName :: Maybe String,
    saveSessionNewUUID :: Maybe Bool
}

instance FromJSON SaveSessionCommand where
    -- parseJSON Null = return $ SaveSessionCommand Nothing Nothing
    parseJSON (Object o) =
        SaveSessionCommand <$> (o .:? "name")
        <*> (o .:? "new_uuid")
    parseJSON _ = mzero
    --parseJSON _ = return $ SaveSessionCommand Nothing Nothing

instance ToJSON SaveSessionCommand where
    toJSON (SaveSessionCommand name new_uuid) =
            object ["name" .= fromMaybe "" name,
                    "new_uuid" .= fromMaybe False new_uuid,
                    "command" .= ("save_model" :: String)]
 
postSaveSessionR :: SessionId -> Handler TypedContent
postSaveSessionR sid = do
    --liftIO $ print "About to get json body"
    --body <- rawRequestBody $$ consume
    --liftIO $ print $ "body is" ++ (show $ S.length $ S.concat body)
    cmd2::Result SaveSessionCommand  <- parseJsonBody
    let cmd = case cmd2 of
                 Error s -> SaveSessionCommand Nothing Nothing
                 Success val -> val

    --response <- getPortResponse (toJSON cmd) sid
    --return response

    getPortResponse (toJSON cmd) sid >>= return
    

    
    --cmd :: SaveSessionCommand <- requireJsonBody
    --liftIO $ print $ ("cmd is here"::String) ++ (show $ toJSON cmd)
    --response <- getPortResponse (toJSON cmd) sid
    --return response


