module Handler.ModelData where
import System.Directory (doesFileExist)
import Data.Text (concat)
import Import


getModelDataR :: ModelUuid -> Handler Html
getModelDataR muid = do
  getModelFile muid "model.data" typeOctet
  -- wd <- wwwDir
  -- let filename = pack $ muid ++ ".model.data"
  -- let model_file = (wd ++ "models/" ++ muid ++ "/model.data")
  -- e <- liftIO $ doesFileExist model_file
  -- if e
  --   then do
  --   addHeader "Content-Disposition" $ Data.Text.concat
  --     ["attachment; filename=\"", filename, "\""]
  --   sendFile typeOctet model_file
  --   else do
  --   notFound



getCompiledModelDataR :: ModelUuid -> Handler Html
getCompiledModelDataR muid = do
  getModelFile muid "compiled.data" typeOctet
  -- wd <- wwwDir
  -- let filename = pack $ muid ++ ".compiled.data"
  -- let model_file = (wd ++ "models/" ++ muid ++ "/compiled.data")
  -- e <- liftIO $ doesFileExist model_file
  -- if e
  --   then do
  --   addHeader "Content-Disposition" $ Data.Text.concat
  --     ["attachment; filename=\"", filename, "\""]
  --   sendFile typeOctet model_file
  --   else do
  --   notFound



getModelDatasetR :: ModelUuid -> Handler Html
getModelDatasetR muid = do
  getModelFile muid "data_set.json" typeJson
  -- wd <- wwwDir
  -- let filename = pack $ muid ++ ".data_set.json"
  -- let image_file = (wd ++ "models/" ++ muid ++ "/data_set.json")
  
  -- e <- liftIO $ doesFileExist image_file
  -- if e
  --   then do
  --   addHeader "Content-Disposition" $ Data.Text.concat
  --     ["attachment; filename=\"", filename, "\""]
  --   sendFile typeJson image_file
  --   else do
  --   notFound

-- Function to take a modelUUID, filename, content type, and serve the
-- file such that a client will download it as {modelUUID}.filename.
-- This is useful for downloading a file from a handler such that the
-- UUID is embedded in the filename.  Will return a notFound error if
-- the file is not found.
--
-- Example: /models/asdf/model.data will be downloaded ad asdf.model.data
-- Example: /models/xxx/compiled.data will be downloaded ad xxx.compiled.data
getModelFile :: ModelUuid -> Text -> ContentType -> Handler Html
getModelFile muid shortName ctype = do
  wd <- wwwDir
  let save_name = muid <> "." <> shortName
  let file_name = (wd <> "models/" <> unpack muid <> "/" <> (unpack shortName))
  
  e <- liftIO $ doesFileExist file_name
  if e
    then do
    addHeader "Content-Disposition" $ Data.Text.concat
      ["attachment; filename=\"", save_name, "\""]
    sendFile ctype file_name
    else notFound


