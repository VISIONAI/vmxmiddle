{-# LANGUAGE ScopedTypeVariables #-}
module Handler.ProcessImage where

import Import
import Helper.Shared
import Helper.VMXTypes

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
    processImageName   :: String,
    processImageImages :: [VMXImage],
    processImageParams   :: VMXParams
}

instance FromJSON ProcessImageCommand where
    parseJSON (Object o) = do
        ProcessImageCommand "weneedtogiveitaname" <$> (o .: "images") <*> (o .: "params")
    parseJSON _ = mzero


postProcessImageR :: SessionId -> Handler String
postProcessImageR sid = do
   addHeader "Access-Control-Allow-Origin" "*"
   addHeader "Content-Type" "application/json"
   (pic :: ProcessImageCommand) <- requireJsonBody
   val <- processImage sid (processImageImages pic) (processImageParams pic) (processImageName pic)
   return val

deleteProcessImageR = deleteVMXSession

deleteVMXSession :: SessionId -> Handler ()
deleteVMXSession sid = do
	-- stop process
	exitVMXServer sid
	delVMXFolder $ "sessions/" <> sid
			
	-- delete session files

print't :: Integer -> Integer ->  IO String
print't start end = printf "Computation time: %0.3f sec\n" $ diff start end

diff ::Integer -> Integer -> Double
diff start end = fromIntegral (end - start) / (10^12)
