module Handler.ModelViewer where

import Import

getModelViewerR :: Handler Html
getModelViewerR = sendFile "text/html" "static/models.html"
