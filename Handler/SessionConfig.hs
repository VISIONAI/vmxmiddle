{-# LANGUAGE ScopedTypeVariables #-}
module Handler.SessionConfig where

import Import
import Helper.Shared

data ConfigCommand2 = ConfigCommand2 {
  configCommand2config :: ConfigCommand
}

instance FromJSON ConfigCommand2 where
    parseJSON (Object o) = do
        ConfigCommand2 <$> (o .: "config")
    parseJSON _ = mzero

instance ToJSON ConfigCommand2 where
  toJSON (ConfigCommand2 c) =
    object ["config" .= c]


data ConfigCommand = ConfigCommand {
    configCommandPretrained :: Maybe String,
    configCommandLogImages :: Maybe Bool,
    configCommandLogMemory :: Maybe Bool,
    configCommandDisplayImages:: Maybe Bool,
    configCommandAllowUrls :: Maybe Bool,
    configCommandReadOnly :: Maybe Bool
}

instance FromJSON ConfigCommand where
    parseJSON (Object o) = do
        ConfigCommand <$> (o .:? "pretrained")
          <*> (o .:? "log_images")
          <*> (o .:? "log_memory")
          <*> (o .:? "display_images")
          <*> (o .:? "allow_urls")
          <*> (o .:? "read_only")
    parseJSON _ = mzero

instance ToJSON ConfigCommand where
  toJSON (ConfigCommand pt li lm di au ro) =
    object ["pretrained" .= pt,
            "log_images" .= li,
            "log_memory" .= lm,
            "display_images" .= di,
            "allow_urls" .= au,
            "read_only" .= ro]

optionsSessionConfigR :: SessionId -> Handler ()
optionsSessionConfigR _ = do
    addHeader "Allow" "Get, Post"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET, POST"
    return ()


postSessionConfigR :: SessionId -> Handler TypedContent
postSessionConfigR sid = do
    (cc :: ConfigCommand2) <- requireJsonBody
     -- trace "start of get session params" $ liftIO $ print sid
    addHeader "Content-Type" "application/json"

    let req = object ["command" .= command,
                      "config" .= (configCommand2config cc)
                      ]
    response <- getPortResponse req sid
    return response
    where
        command :: String
        command = "config"


getSessionConfigR :: SessionId -> Handler TypedContent
getSessionConfigR sid = do
     -- trace "start of get session params" $ liftIO $ print sid
    addHeader "Content-Type" "application/json"
    let req = object ["command" .= command]
    response <- getPortResponse req sid
    return response
    where
        command :: String
        command = "config"

