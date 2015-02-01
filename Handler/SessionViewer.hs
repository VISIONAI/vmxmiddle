{-|
Module      : Handler.SessionViewer
Description : sessions.html

Serves sessions.html
-}
module Handler.SessionViewer where

import Import

{-|
Serves sessions.html
-}
getSessionViewerR :: Handler Html
getSessionViewerR = sendFile "text/html" "static/sessions.html"
