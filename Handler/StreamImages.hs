module Handler.StreamImages where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

import System.Random
import Data.IORef (readIORef, writeIORef, IORef)
import Data.Map (member, (!), Map)
import qualified Data.Map as M (insert)


import Import

--getStreamImagesR :: ModelUuid -> Handler Html
--getStreamImagesR = error "Not yet implemented: getStreamImagesR"

pick :: [a] -> IO a
pick xs = do
  index <- randomRIO (0, (length xs - 1))
  return (xs !! index)
  
fileEnding :: String -> String
fileEnding x = reverse $ take 4 $ reverse x

last9 :: forall a. [a] -> [a]
last9 x = reverse $ take 9 $ reverse x


getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

-- optionsRandomImageR :: Handler ()
-- optionsRandomImageR = do
--     addHeader "Allow" "GET"
--     addHeader "Access-Control-Allow-Origin" "*"
--     addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
--     addHeader "Access-Control-Allow-Methods" "GET"
--     return ()

seedRecursiveContentsCache :: ModelUuid -> IORef (Map Text [FilePath]) -> Handler [FilePath]
seedRecursiveContentsCache muid cacheRef = do
      modelsDir <- (++ ("models/" ++ unpack muid)) <$> wwwDir
      modelFolders' <- liftIO $ getRecursiveContents modelsDir
      let modelFolders = filter (\x -> ".jpg" == fileEnding x) modelFolders'
      let modelFoldersClean = filter (\x -> "image.jpg" /= last9 x) modelFolders

      oldCache <- liftIO $ readIORef cacheRef
      liftIO $ writeIORef cacheRef $ M.insert muid modelFoldersClean oldCache

      return modelFoldersClean
    
postResetCacheR :: ModelUuid -> Handler ()
postResetCacheR mid = do
    App _ _ _ _ _ _ _ _ modelImageCache' <- getYesod
    seedRecursiveContentsCache mid modelImageCache'
    _ <- seedRecursiveContentsCache mid modelImageCache'
    return ()

-- This handler will return a random image from the modelsDir
-- directory, and an error is thrown if there are 0 images.
getStreamImagesR :: ModelUuid -> Handler Html
getStreamImagesR muid = do
  App _ _ _ _ _ _ _ _ modelImageCache' <- getYesod
  modelImageCache <- liftIO . readIORef $ modelImageCache'
  modelFoldersClean <- 
        if member muid modelImageCache 
            then return $ modelImageCache ! muid
            else seedRecursiveContentsCache muid modelImageCache'
  
  let l = length modelFoldersClean
  if l==0
    then error "Not enough images inside model folder, go train some models!"
    else do
      randfile <- liftIO $ pick modelFoldersClean
      sendFile "image/jpg" randfile

getStreamImagesFirstR :: ModelUuid -> Handler Html
getStreamImagesFirstR muid = do
  modelDir <- (++ ("models/" ++ unpack muid ++ "/data_set/")) <$> wwwDir
  sendFile "image/jpg" (modelDir ++ "000001.jpg")


getStreamImagesNextR :: ModelUuid -> Handler Html
getStreamImagesNextR muid = do
  App _ _ _ _ _ _ _ _ modelImageCache' <- getYesod
  modelImageCache <- liftIO . readIORef $ modelImageCache'
  modelFoldersClean <- 
        if member muid modelImageCache 
            then return $ modelImageCache ! muid
            else seedRecursiveContentsCache muid modelImageCache'

  let images = modelFoldersClean
  mbIndex <- lookupSession $ muid
  let index = maybe 0 (read . unpack) mbIndex
  let safeIndex = index `mod` length images
  setSession muid $ pack $ show $ safeIndex + 1
  sendFile "image/jpg" $ (images !! safeIndex)
