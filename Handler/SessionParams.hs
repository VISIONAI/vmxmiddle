{-|
Module      : SessionParams
Description : VMX Session Parameters

Gets the VMX session parameters
-}
module Handler.SessionParams where

import Import
import Helper.Shared

{-|
OPTIONS for \/session\/#SessionId\/param
-}
optionsSessionParamsR :: SessionId -> Handler ()
optionsSessionParamsR _ = do
    addHeader "Allow" "Get"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET"
    return ()

{-|
GET for \/session\/#SessionId\/param

Lits the parameters for the current session
-}
getSessionParamsR :: SessionId -> Handler TypedContent
getSessionParamsR sid = do
    addHeader "Access-Control-Allow-Origin" "*"
     -- trace "start of get session params" $ liftIO $ print sid
    addHeader "Content-Type" "application/json"
    let req = object ["command" .= command]
    response <- getPortResponse req sid
    return response
    where
        command :: String
        command = "get_params"

