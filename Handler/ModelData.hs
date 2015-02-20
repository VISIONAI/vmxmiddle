module Handler.ModelData where
import System.Directory (doesFileExist)
import Import


getModelDataR :: ModelUuid -> Handler Html
getModelDataR muid = do
  wd <- wwwDir
  let model_file = (wd ++ "models/" ++ muid ++ "/model.data")
  let default_file = "static/img/missing.jpg"
  e <- liftIO $ doesFileExist model_file
  if e
    then do
    --liftIO $ print "sending real image.jpg"
    sendFile typePlain model_file
    else do
    --liftIO $ print "sending missing.jpg"
    sendFile "image/png" default_file


getCompiledModelDataR :: ModelUuid -> Handler Html
getCompiledModelDataR muid = do
  wd <- wwwDir
  let model_file = (wd ++ "models/" ++ muid ++ "/compiled.data")
  let default_file = "static/img/missing.jpg"
  e <- liftIO $ doesFileExist model_file
  if e
    then do
    --liftIO $ print "sending real image.jpg"
    sendFile typePlain model_file
    else do
    --liftIO $ print "sending missing.jpg"
    sendFile "image/png" default_file

