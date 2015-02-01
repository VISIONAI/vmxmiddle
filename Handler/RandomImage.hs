{-|
Module      : RandomImage
Description : Serving random images from VMX

This module contains functions for serving random images from inside
all of the VMX model folder. NOTE: this function is highly inefficient
and takes quite some time to process, making it quite un-usable as an
IP camera.
-}
module Handler.RandomImage where

import Import
import Helper.Shared

{-|
OPTIONS for \/random
-}
optionsRandomImageR :: Handler ()
optionsRandomImageR = do
    addHeader "Allow" "GET"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET"
    return ()

{-|
GET \/random

This handler will return a random image from the models
directory, and an error is thrown if there are 0 images.
-}
getRandomImageR :: Handler Html
getRandomImageR = do
  modelsDir <- (++ "models/") <$> wwwDir
  modelFolders' <- liftIO $ getRecursiveContents modelsDir

  let modelFolders = filter (\x -> ".jpg" == fileEnding x) modelFolders'
  let modelFoldersClean = filter (\x -> "image.jpg" /= last9 x) modelFolders

  let l = length modelFoldersClean
  if l==0
    then error "Not enough images inside model folder, go train some models!"
    else do
      randfile <- liftIO $ pickRandom modelFoldersClean
      sendFile "image/jpg" randfile
