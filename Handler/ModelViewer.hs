{-|
Module      : Handler.ModelViewer
Description : models.html

Serves models.html
-}
module Handler.ModelViewer where

import Import

{-|
Returns models.html
-}
getModelViewerR :: Handler Html
getModelViewerR = sendFile "text/html" "static/models.html"
