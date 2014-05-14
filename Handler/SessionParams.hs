module Handler.SessionParams where

import Import
import Helper.Shared



getSessionParamsR :: SessionId -> Handler String
getSessionParamsR sid = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Content-Type" "application/json"
    liftIO $ print sid
    let req = object ["command" .= update_params, "session_id" .= ("sessions/" ++ sid)]
    response <- getPipeResponse req sid
    return response
    where
        update_params :: String
        update_params = "update_params"

