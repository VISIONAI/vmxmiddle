{-|
Module      : Handler.Home
Description : index.html

Serves index.html
-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = sendFile "text/html" "static/index.html"

