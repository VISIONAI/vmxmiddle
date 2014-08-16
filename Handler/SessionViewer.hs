module Handler.SessionViewer where

import Import

getSessionViewerR :: Handler Html
getSessionViewerR = sendFile "text/html" "static/sessions.html"
