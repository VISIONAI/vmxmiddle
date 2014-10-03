module Handler.Forward where

import Import
import Network.HTTP (getRequest, getResponseBody, simpleHTTP, Request, mkRequest)
import Network.HTTP( RequestMethod( GET ) )
import qualified Data.ByteString as B
import Data.Maybe (fromJust)
import Network.URI (parseURI)

-- useful example found at
-- https://github.com/sellweek/xkcd/blob/7baf85dd12d17601cb238e0eb5d408ef82320097/src/XKCD.hs

-- other useful stuff at https://github.com/andreyk0/www-webcam-snapshot/blob/master/Main.hs

-- |Downloads file from URL and returns its contents as a 'B.ByteString'
downloadFile :: String -> IO B.ByteString
downloadFile url = response >>= getResponseBody
  where 
    response = simpleHTTP ((mkRequest GET $ fromJust $ parseURI url) :: Request B.ByteString)

getForwardR :: Url -> Handler String
getForwardR url' = do 
  let url = "http://" ++ url'
  f <- liftIO $ downloadFile url
  sendResponse (typeJpeg, toContent f)
  
  -- let myRequest = Network.HTTP.getRequest url
  -- liftIO $ print $ "URL is " ++ url
  -- res <- liftIO $ simpleHTTP myRequest >>= getResponseBody
  -- return res
