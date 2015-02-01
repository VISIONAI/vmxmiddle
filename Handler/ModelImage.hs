{-|
Module      : ModelImage
Description : Returns an image for a model UUID

This module contains the function for returning the model image.
-}
module Handler.ModelImage where

import System.Directory (doesFileExist)
import Import

{-|
GET \/models\/#id\/image.jpg

Returns a mean image representing the model
-}
getModelImageR :: ModelId -> Handler Html
getModelImageR muid = do
  wd <- wwwDir
  let image_file = (wd ++ "models/" ++ muid ++ "/image.jpg")
  let default_file = "static/img/missing.jpg"
  e <- liftIO $ doesFileExist image_file
  if e
    then do
    --liftIO $ print "sending real image.jpg"
    sendFile "image/jpg" image_file
    else do
    --liftIO $ print "sending missing.jpg"
    sendFile "image/jpg" default_file

