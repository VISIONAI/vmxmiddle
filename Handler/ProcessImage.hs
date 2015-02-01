{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : ProcessImage
Description : VMX Object Detection

Processing an image means applying a model to an image and producing
a list of object detections.
-}
module Handler.ProcessImage where

import Import
import Helper.Shared
import Helper.VMXTypes
import System.Directory (removeDirectoryRecursive)

{-|
OPTIONS for \/session\/#SessionId
-}
optionsProcessImageR :: SessionId -> Handler ()
optionsProcessImageR _ = do
    addHeader "Allow" "POST"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "POST"
    return ()

{-|
Data structure for processing an image: an object name (currently
unused), an array of images (currently we are only using one), and 'VMXParams'
-}
data ProcessImageCommand =  ProcessImageCommand {
    processImageName   :: String,
    processImageImages :: [VMXImage],
    processImageParams :: VMXParams
}

-- NOTE from TJM: "weeneedtogiveitname" is probably here because we
-- can in theory give a process image command the name of the objects
-- we only care about.. this make sense if we've loaded a bunch of
-- models into one session, but only want the responses for "hand" and
-- not the remaining "100 objects.  Currently we aren't doing this...
instance FromJSON ProcessImageCommand where
    parseJSON (Object o) = do
        ProcessImageCommand "" <$> (o .: "images") <*> (o .: "params")
    parseJSON _ = mzero

{-|
POST \/session\/#SessionId

Performs detection on an image
-}
postProcessImageR :: SessionId -> Handler TypedContent
postProcessImageR sid = do
   addHeader "Access-Control-Allow-Origin" "*"
   addHeader "Content-Type" "application/json"
   (pic :: ProcessImageCommand) <- requireJsonBody
   val <- processImage sid (processImageImages pic) (processImageParams pic) (processImageName pic)
   return val

{-|
Process an Image
-}
processImage :: SessionId -> [VMXImage] -> VMXParams -> String -> Handler TypedContent
processImage sid image params name = do
   let req = object ["command" .= command, "name" .= name, "images" .= image, "params" .= params]
   response <- getPortResponse req sid
   return response
   where
        command :: String
        command = "process_image"

{-|
Stop the VMX server session and process
-}
exitVMXServer :: SessionId -> Handler TypedContent
exitVMXServer sid = getPortResponse (object ["command" .= exit]) sid >>=  return
	where 	
		exit :: String
		exit = "exit"


{-|
DELETE \/session\/#SessionId

Removes the current session (does not save the model first)
-}
deleteProcessImageR :: SessionId -> Handler ()
deleteProcessImageR sid = do
	_ <- exitVMXServer sid
	delVMXFolder $ "sessions/" <> sid

{-|
Recursively delete a VMX folder
-}
delVMXFolder :: FilePath -> Handler ()
delVMXFolder fp = do
	wwwDir' <- wwwDir
	liftIO $ removeDirectoryRecursive $ wwwDir' <> fp >>= return
