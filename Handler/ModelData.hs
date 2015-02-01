{-|
Module      : ModelData
Description : VMX ModelData

Returns the model data, which is a model.data file containing the
model.  This allows for another VMXserver running on a different
machine to import this model.
-}
module Handler.ModelData where
import System.Directory (doesFileExist)
import Import

{-|
GET \/models\/#id\/model.data

Serves the \/models\/uuid\/model.data file, or a missing.jpg file
-}
getModelDataR :: ModelId -> Handler Html
getModelDataR muid = do
  wd <- wwwDir
  let model_file = (wd ++ "models/" ++ muid ++ "/model.data")
  let default_file = "static/img/missing.jpg"
  e <- liftIO $ doesFileExist model_file
  if e
    then do
    sendFile typePlain model_file
    else do
    sendFile "image/jpg" default_file

