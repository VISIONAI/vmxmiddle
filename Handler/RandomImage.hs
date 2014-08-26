module Handler.RandomImage where


import Import


import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

import System.Random


getRandom a = do {
  x <- randomRIO(0,100);
  return x;
  }

-- Import System.Random (randomRIO)

pick :: [a] -> IO a
pick xs = do
  index <- randomRIO (0, (length xs - 1))
  return (xs !! index)


-- rnd_select xs n = mapM f [1 .. n] where
--   f = const $ r xs
--   r xs = do
--     index <- randomRIO (0, length xs - 1)
--     return (xs !! index)
  
fileEnding x = reverse $ take 4 $ reverse x


last9 x = reverse $ take 9 $ reverse x



getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames' = filter (`notElem` [".", ".."]) names
  let properNames = properNames'
  -- let properNames = filter (\x -> ".jpg" == fileEnding x) properNames'
  -- let properNames = filter (\x -> (reverse (take 4 (reverse x)) == ".jpg")) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

optionsRandomImageR :: Handler ()
optionsRandomImageR = do
    addHeader "Allow" "GET"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "GET"
    return ()


getRandomImageR :: Handler Html
getRandomImageR = do
  modelsDir <- (++ "models/") <$> wwwDir
  modelFolders' <- liftIO $ getRecursiveContents modelsDir

  let modelFolders = filter (\x -> ".jpg" == fileEnding x) modelFolders'
  let modelFoldersClean = filter (\x -> "image.jpg" /= last9 x) modelFolders
  -- liftIO $ print "hello there"
  -- liftIO $ print modelFolders
  -- liftIO $ print getRandom
  randfile <- liftIO $ pick modelFoldersClean
  -- randfile :: FilePath <- pick modelFolders
   -- r2 <- randfile
  -- files <- getDirectoryContents "."
  --let randfile = "/www/vvv2/models/5e6fd119-aa19-491b-9911-999a81c22034/data_set/000013.jpg"
  sendFile "image/jpg" randfile
