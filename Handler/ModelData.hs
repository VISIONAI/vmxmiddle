module Handler.ModelData where
import System.Directory (doesFileExist)
import Data.Text (concat)
import Import


getModelDataR :: ModelId -> Handler Html
getModelDataR muid = do
  wd <- wwwDir
  let filename = pack $ muid ++ ".model.data"
  let model_file = (wd ++ "models/" ++ muid ++ "/model.data")
  let default_file = "static/img/missing.jpg"
  e <- liftIO $ doesFileExist model_file
  if e
    then do
    --liftIO $ print "sending real image.jpg"
    addHeader "Content-Disposition" $ Data.Text.concat
      ["attachment; filename=\"", filename, "\""]
    sendFile typeOctet model_file
    else do
    --liftIO $ print "sending missing.jpg"
    sendFile "image/png" default_file


getCompiledModelDataR :: ModelId -> Handler Html
getCompiledModelDataR muid = do
  wd <- wwwDir
  let filename = pack $ muid ++ ".compiled.data"
  let model_file = (wd ++ "models/" ++ muid ++ "/compiled.data")
  let default_file = "static/img/missing.jpg"
  e <- liftIO $ doesFileExist model_file
  if e
    then do
    --liftIO $ print "sending real image.jpg"
    addHeader "Content-Disposition" $ Data.Text.concat
      ["attachment; filename=\"", filename, "\""]
    sendFile typeOctet model_file

    else do
    --liftIO $ print "sending missing.jpg"
    sendFile "image/png" default_file

