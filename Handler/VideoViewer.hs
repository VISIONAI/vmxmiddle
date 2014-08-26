module Handler.VideoViewer where

import Import

getVideoViewerR :: Handler Html
getVideoViewerR = sendFile "text/html" "static/video.html"

