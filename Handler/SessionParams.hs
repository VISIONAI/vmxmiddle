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
    let req = object ["command" .= command]
    response <- getPortResponse req sid
    return response
    where
        command :: String
        command = "params"

