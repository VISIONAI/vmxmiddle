{-|
Module      : Handler.VideoViewer
Description : video.html

Serves video.html
-}
module Handler.VideoViewer where

import Import

{-|
Serves video.html
-}
getVideoViewerR :: Handler Html
getVideoViewerR = sendFile "text/html" "static/video.html"

