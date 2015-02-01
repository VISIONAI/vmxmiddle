{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : LoadModel
Description : VMX Model Loading

This module contains functions for loading a model into a current session
-}
module Handler.LoadModel where

import Import
import Helper.Shared

{-|
A 'LoadModelCommand' will load a model into a session.  Requires a
list of UUIDs and a boolean flag indicating if we want to load
compiled model(s).
-}
data LoadModelCommand =  LoadModelCommand {
    loadModelUuids      :: [String],
    loadModelCompiled   :: Bool
}

instance FromJSON LoadModelCommand where
    parseJSON (Object o) = 
        LoadModelCommand <$> (o .: "uuids") <*> (o .: "compiled")
    parseJSON _ = mzero

{-|
OPTIONS for \/session\/#SessionId\/load
-}
optionsLoadModelR :: SessionId -> Handler ()
optionsLoadModelR _ = do
    addHeader "Allow" "POST"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET"
    return ()

{-|
POST \/session\/#SessionId\/load

Loads a Model
-}
postLoadModelR :: SessionId -> Handler TypedContent
postLoadModelR sid' = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Content-Type" "application/json"
  (pic :: LoadModelCommand) <- requireJsonBody
  val <- loadModel sid' (loadModelUuids pic) (loadModelCompiled pic)
  return val
  where
    loadModel :: SessionId -> [String] -> Bool -> Handler TypedContent
    loadModel sid uuids compiled = do
      let req = object ["command" .= command, "uuids" .= uuids, "compiled" .= compiled]
      response <- getPortResponse req sid
      return response
      where
        command :: String
        command = "load_model"
