module Handler.ModelImage where
import System.Directory (doesFileExist)
import Import



getModelImageR :: ModelUuid -> Handler Html
getModelImageR muid = do
  wd <- wwwDir
  let image_file = (wd <> "models/" <> unpack muid <> "/image.jpg")
  let default_file = "static/img/missing.jpg"
  e <- liftIO $ doesFileExist image_file
  if e
    then do
    --liftIO $ print "sending real image.jpg"
    sendFile "image/jpg" image_file
    else do
    --liftIO $ print "sending missing.jpg"
    sendFile "image/png" default_file


