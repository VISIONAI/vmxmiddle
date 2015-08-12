module Handler.Forward where

import Import
import Network.HTTP (getResponseBody, simpleHTTP, Request, mkRequest)
import Network.HTTP( RequestMethod( GET ) )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (fromJust)
--import Network.URI (parseURI)
import Data.List (isPrefixOf)
import Control.Monad                ((<=<))
import qualified Data.ByteString.Lazy.Char8 as  LZ
import Handler.CheckLicense
import Control.Exception (tryJust)
import System.IO.Error (isDoesNotExistError)
import Control.Monad (guard)
import qualified Data.ByteString.Char8 as C (pack)
import System.Info
import Data.Text.IO (hGetContents)
import System.Process
-- useful example found at
-- https://github.com/sellweek/xkcd/blob/7baf85dd12d17601cb238e0eb5d408ef82320097/src/XKCD.hs
-- other useful stuff at https://github.com/andreyk0/www-webcam-snapshot/blob/master/Main.hs

import Network.HTTP.Conduit

-- | make a simple http request but add a user agent to the HTTP header
-- You HAVE TO add a User-Agent in your header to use the github API.
simpleHTTPWithUserAgent :: String -> [Char] -> [Char] -> IO LZ.ByteString
simpleHTTPWithUserAgent url version osv = do
    r  <- parseUrl url

    let uuid' = C.pack $ concat [version, " (", os, osv, " ", arch, " https://vision.ai)"]
    let request = r { requestHeaders =  [ ("User-Agent",uuid') ] }
    withManager $ (return.responseBody) <=< httpLbs request

-- Downloads a file from URL and returns its contents as a 'B.ByteString'
-- Allows for http and https downloads
downloadFile :: String -> String -> String -> IO B.ByteString
downloadFile url uuid osv = do
  -- b <- simpleHttp url
  b <- simpleHTTPWithUserAgent url uuid osv
  return $ B.concat $ LB.toChunks b

getForwardR :: Handler String
getForwardR = do
  urlMaybe <- lookupGetParam "q"
  e <- liftIO $ tryJust (guard . isDoesNotExistError) (readFile "version")
  let versionMiddle = either (const "development") id e

  osv <- case os of
    "darwin" -> do
      (_, Just hout, _, hdl) <- liftIO $ createProcess (proc "sw_vers" ["-productVersion"]){ std_out = CreatePipe }
      osv <- liftIO $ Data.Text.IO.hGetContents hout
      exitCode <- liftIO $ waitForProcess hdl
      return osv
    _ -> do
      (_, Just hout, _, hdl) <- liftIO $ createProcess (proc "uname" ["-mrs"]){ std_out = CreatePipe }
      osv <- liftIO $ Data.Text.IO.hGetContents hout
      exitCode <- liftIO $ waitForProcess hdl
      return osv

  case urlMaybe of
    Nothing -> do
      error "missing q param"
    Just u -> do
      let url = add_prefix $ unpack u
      let osv' = (lines $ unpack $ osv) !! 0
      f <- liftIO $ downloadFile url versionMiddle osv'
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
      
           
             
