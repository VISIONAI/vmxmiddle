{-# LANGUAGE ScopedTypeVariables #-}
module Handler.ProcessImage where

import Import
import Helper.Shared

import System.CPUTime
import Text.Printf


optionsProcessImageR :: SessionId -> Handler ()
optionsProcessImageR _ = do
    addHeader "Allow" "POST"
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Headers" "Authorization,Content-Type"
    addHeader "Access-Control-Allow-Methods" "POST"
    return ()

data ProcessImageCommand =  ProcessImageCommand {
    picImage :: String,
    processImageParams   :: Value,
    picTime :: Int
}

instance FromJSON ProcessImageCommand where
    parseJSON (Object o) = do
        ProcessImageCommand <$> (o .: "image") <*> (o .: "params") <*> (o .: "time")
    parseJSON _ = mzero


postProcessImageR :: SessionId -> Handler String
postProcessImageR sid = do
   addHeader "Access-Control-Allow-Origin" "*"
   addHeader "Content-Type" "application/json"
   start <- liftIO $ getCPUTime
   (pic :: ProcessImageCommand) <- requireJsonBody
   after_parse <- liftIO $ getCPUTime
   liftIO $ print "time to parse request (in httppost)"
   liftIO $ print't start after_parse
   val <- processImage sid (picImage pic) (processImageParams pic) (picTime pic)
   after_process <- liftIO $ getCPUTime
   liftIO $ print "time to process_image"
   liftIO $ print't after_parse after_process
   return val

print't :: Integer -> Integer ->  IO String
print't start end = printf "Computation time: %0.3f sec\n" $ diff start end

diff ::Integer -> Integer -> Double
diff start end = fromIntegral (end - start) / (10^12)
