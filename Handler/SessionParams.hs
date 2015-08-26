module Handler.SessionParams where

import Import
import Helper.Shared

optionsSessionParamsR :: SessionId -> Handler ()
optionsSessionParamsR _ = do
    addHeader "Allow" "Get"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET"
    return ()


getSessionParamsR :: SessionId -> Handler TypedContent
getSessionParamsR sid = do
     -- trace "start of get session params" $ liftIO $ print sid
    addHeader "Content-Type" "application/json"
    let req = object ["command" .= command]
    response <- getPipeResponse req sid
    return response
    where
        command :: String
        command = "get_params"

