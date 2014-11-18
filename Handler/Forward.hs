module Handler.Forward where

import Import
import Network.HTTP (getResponseBody, simpleHTTP, Request, mkRequest)
import Network.HTTP( RequestMethod( GET ) )
import qualified Data.ByteString as B
import Data.Maybe (fromJust)
import Network.URI (parseURI)


-- useful example found at
-- https://github.com/sellweek/xkcd/blob/7baf85dd12d17601cb238e0eb5d408ef82320097/src/XKCD.hs
-- other useful stuff at https://github.com/andreyk0/www-webcam-snapshot/blob/master/Main.hs

-- Downloads a file from URL and returns its contents as a 'B.ByteString'
downloadFile :: String -> IO B.ByteString
downloadFile url = response >>= getResponseBody
  where
--    response = simpleHttp ((mkRequest GET $ fromJu) :: Request B.ByteString)
    response = simpleHTTP ((mkRequest GET $ fromJust $ parseURI url) :: Request B.ByteString)

getForwardR :: Handler String
getForwardR = do
  urlMaybe <- lookupGetParam "q"
  -- getParameters <- reqGetParams <$> Import.getRequest
--  liftIO $ print $ concatMap ("Params: " ++) getParameters
  -- Import.getRequest >>= liftIO . print . ("Params :" ++) . concatMap show . reqGetParams
  case urlMaybe of
    Nothing -> do
      error "missing q param"
    Just u -> do
      let url = unpack u
      -- liftIO $ print ("URL is " ++ url)
      liftIO $ print ("Downloading " ++ url)
      liftIO $ print $ fromJust $ parseURI url
      f <- liftIO $ downloadFile url
      sendResponse (typeJpeg, toContent f)
