{-|
Module      : StreamImages
Description : VMX Model directory streaming

Streams images from the models directory, together with some logic to
keep track of the 'next' image for each model directory.
-}
module Handler.StreamImages where

import Data.IORef (readIORef, writeIORef, IORef)
import Data.Map (member, (!), Map)
import qualified Data.Map as M (insert)
import Helper.Shared
import Import


{-|
Seeds the content cache so that we don't have to list images on every
request
-}
seedRecursiveContentsCache :: ModelId -> IORef (Map String [String]) -> Handler [String]
seedRecursiveContentsCache muid cacheRef = do
      modelsDir <- (++ ("models/" ++ muid)) <$> wwwDir
      modelFolders' <- liftIO $ getRecursiveContents modelsDir
      let modelFolders = filter (\x -> ".jpg" == fileEnding x) modelFolders'
      let modelFoldersClean = filter (\x -> "image.jpg" /= last9 x) modelFolders
      oldCache <- liftIO $ readIORef cacheRef
      liftIO $ writeIORef cacheRef $ M.insert muid modelFoldersClean oldCache
      return modelFoldersClean


{-|
POST \/models\/#ModelId\/reset_cache

Resets the image cache which is used to keep track of which image is
'next'
-}
postResetCacheR :: ModelId -> Handler ()
postResetCacheR mid = do
    App _ _ _ _ _ _ modelImageCache' <- getYesod
    _ <- seedRecursiveContentsCache mid modelImageCache'
    return ()

{-|
GET \/models\/#ModelId\/random.jpg

This handler will return a random image from the models
directory, and an error is thrown if there are 0 images.
-}
getStreamImagesR :: ModelId -> Handler Html
getStreamImagesR muid = do
  App _ _ _ _ _ _ modelImageCache' <- getYesod
  modelImageCache <- liftIO . readIORef $ modelImageCache'
  modelFoldersClean <- 
        if member muid modelImageCache 
            then return $ modelImageCache ! muid
            else seedRecursiveContentsCache muid modelImageCache'
  
  let l = length modelFoldersClean
  if l==0
    then error "Not enough images inside model folder, go train some models!"
    else do
      randfile <- liftIO $ pickRandom modelFoldersClean
      sendFile "image/jpg" randfile

{-|
GET \/models\/#ModelId\/first.jpg

This handler will return the first image from a models directory
-}
getStreamImagesFirstR :: ModelId -> Handler Html
getStreamImagesFirstR muid = do
  modelDir <- (++ ("models/" ++ muid ++ "/data_set/")) <$> wwwDir
  sendFile "image/jpg" (modelDir ++ "000001.jpg")

{-|
GET \/models\/#ModelId\/image.jpg

This handler will return the next image in an ordered way.  This
function will also create the cache in case it hasn't been yet
created.
-}
getStreamImagesNextR :: ModelId -> Handler Html
getStreamImagesNextR muid = do
  App _ _ _ _ _ _ modelImageCache' <- getYesod
  modelImageCache <- liftIO . readIORef $ modelImageCache'
  modelFoldersClean <- 
        if member muid modelImageCache 
            then return $ modelImageCache ! muid
            else seedRecursiveContentsCache muid modelImageCache'

  let images = modelFoldersClean
  mbIndex <- lookupSession $ pack muid
  let index = maybe 0 (read . unpack) mbIndex
  let safeIndex = index `mod` length images
  setSession (pack muid) $ pack $ show $ safeIndex + 1
  sendFile "image/jpg" $ (images !! safeIndex)
