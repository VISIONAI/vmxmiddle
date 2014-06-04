module Handler.SessionParams where

import Import
import Helper.Shared

optionsSessionParamsR :: SessionId -> Handler ()
optionsSessionParamsR _ = do
    addHeader "Allow" "Get"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET"
    return ()


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

