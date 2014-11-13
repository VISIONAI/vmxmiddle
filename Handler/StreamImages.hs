module Handler.StreamImages where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

import System.Random


import Import

--getStreamImagesR :: ModelId -> Handler Html
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
  let properNames' = filter (`notElem` [".", ".."]) names
  let properNames = properNames'
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

-- This handler will return a random image from the modelsDir
-- directory, and an error is thrown if there are 0 images.
getStreamImagesR :: ModelId -> Handler Html
getStreamImagesR muid = do
  modelsDir <- (++ ("models/" ++ muid)) <$> wwwDir
  modelFolders' <- liftIO $ getRecursiveContents modelsDir

  let modelFolders = filter (\x -> ".jpg" == fileEnding x) modelFolders'
  let modelFoldersClean = filter (\x -> "image.jpg" /= last9 x) modelFolders

  let l = length modelFoldersClean
  if l==0
    then error "Not enough images inside model folder, go train some models!"
    else do
      randfile <- liftIO $ pick modelFoldersClean
      sendFile "image/jpg" randfile

getStreamImagesFirstR :: ModelId -> Handler Html
getStreamImagesFirstR muid = do
  modelDir <- (++ ("models/" ++ muid ++ "/data_set/")) <$> wwwDir
  sendFile "image/jpg" (modelDir ++ "000001.jpg")


getStreamImagesNextR :: ModelId -> Handler Html
getStreamImagesNextR muid = do
  modelsDir <- (++ ("models/" ++ muid ++ "/data_set/")) <$> wwwDir
  modelFolders' <- liftIO $ getRecursiveContents modelsDir

  let modelFolders = filter (\x -> ".jpg" == fileEnding x) modelFolders'

  mname <- lookupSession $ pack muid
  
  case mname of
        Nothing -> do
            setSession (pack muid) "0"
        Just _ -> liftIO $ print $ "" 
  -- QUESTION FROM TOM: how to I place an empty statement above instead of a silly print nothing
        
  token <- lookupSession $ pack muid
  case token of
    Just realtoken -> do
      let index = read (unpack realtoken)::Int
      if index == length modelFolders
        then do setSession (pack muid) "0"
                sendFile "image/jpg" $ (modelFolders !! 0)
        else do let newone = (show (index + 1))::String
                setSession (pack muid) $ (pack newone)
                sendFile "image/jpg" $  (modelFolders !! index)
    --Nothing -> do
    --  liftIO $ print "Never gets here"
