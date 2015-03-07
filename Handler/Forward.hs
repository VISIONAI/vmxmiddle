module Handler.Forward where

import Import
import Network.HTTP (getResponseBody, simpleHTTP, Request, mkRequest)
import Network.HTTP( RequestMethod( GET ) )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import Data.List (isPrefixOf)

-- useful example found at
-- https://github.com/sellweek/xkcd/blob/7baf85dd12d17601cb238e0eb5d408ef82320097/src/XKCD.hs
-- other useful stuff at https://github.com/andreyk0/www-webcam-snapshot/blob/master/Main.hs

import Network.HTTP.Conduit

-- Downloads a file from URL and returns its contents as a 'B.ByteString'
-- Allows for http and https downloads
downloadFile :: String -> IO B.ByteString
downloadFile url = do
  b <- simpleHttp url
  return $ B.concat $ LB.toChunks b

getForwardR :: Handler String
getForwardR = do
  urlMaybe <- lookupGetParam "q"
  case urlMaybe of
    Nothing -> do
      error "missing q param"
    Just u -> do
      let url = add_prefix $ unpack u
      
      -- liftIO $ print ("URL is " ++ url)
      liftIO $ print ("Downloading " ++ url)
      liftIO $ print $ fromJust $ parseURI url
      f <- liftIO $ downloadFile url
      sendResponse (typeJpeg, toContent f)
  where
    add_prefix :: [Char] -> [Char]
    add_prefix a = do
      let clean = isPrefixOf "http" a
      case clean of
        True -> do
          a
        False -> do
          concat ["http://", a]
      
           
             
