{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.WebSocket (getWebSocketR) where

import Import
import Helper.Shared
import Helper.VMXTypes
import Yesod.WebSockets
import Control.Monad(forever)
import Control.Concurrent.STM
import Data.Text (reverse)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as H

import Handler.Session
import Handler.Model
import qualified Data.ByteString.Lazy as L
import Control.Applicative ((<|>),empty)
import qualified Data.ByteString as B

import System.CPUTime
import Text.Printf


instance FromJSON VMXCommand where
  parseJSON j = do
      o <- {-# SCC "parse_vmxcommand" #-} parseJSON  j
      case H.toList (o :: Object) of
          [("new_connection", Object o')]  -> CreateSession <$> o' .: "uuids"
          [("process_image",   Object o')] -> ProcessImage <$> (o' .: "session_id") <*> (o' .: "image") <*> (o' .: "params")
          [("list_sessions",   Object o')] -> return GetSessions
          _                      -> fail "Rule: unexpected format"


data VMXCommand = CreateSession [String]
                | GetSessions 
                | ProcessImage SessionId [VMXImage] VMXParams
               -- {
               --     piSid'    :: SessionId
               --     picImage' :: String,
               --     processImageParams'   :: Value,
               --     picTime' :: Int
               -- }


vmxWebSocket :: WebSocketsT Handler ()
vmxWebSocket = {-# SCC "dowebsocketforever" #-}(forever $ {-# SCC "insideforever" #-} 
                    do 
                        start <- liftIO $ getCPUTime
                        (d :: B.ByteString) <-                  {-# SCC "receiveData" #-}      receiveData 
                        after_receive <- liftIO $ getCPUTime
                        liftIO $ print "time to receiveData"
                        liftIO $ print't start after_receive
                        let (wsc :: Either String VMXCommand) = {-# SCC "decode_websocket" #-} eitherDecode' $ L.fromChunks [d]
                        after_decode <- liftIO $ getCPUTime
                        before_case <- liftIO $ getCPUTime 
                        case  wsc of
                            Left error' -> {-# SCC "sendError" #-} sendTextData $ pack error'
                            Right command' -> {-# SCC "parse_command" #-}
                                case command' of
                                    CreateSession model_name -> do
                                        (out :: String) <- lift $ createSession model_name
                                        sendTextData $ pack $ "{\"new_connection\":" <> out <> "}"
                                    ProcessImage sid images params -> do
                                        before_process_image <- liftIO $ getCPUTime
                                        (out :: String) <- {-# SCC "processImage" #-} lift $ processImage sid images params "weneedtogiveitaname"
                                        after_process_image <- liftIO $ getCPUTime
                                        liftIO $ print "time to process image"
                                        liftIO $ print't before_process_image after_process_image
                                        {-# SCC "pack_output" #-} sendTextData $ pack $"{\"process_image\" : " <> out <> "}"
                                        after_send_text <- liftIO $ getCPUTime
                                        return ()
                                        
                        after_case <- liftIO $ getCPUTime
                        liftIO $ print "whole websocket loop?"
                        liftIO $ print't start after_case
                        return ()
                            
                        )


print't :: Integer -> Integer ->  IO String
print't start end = printf "Computation time: %0.3f sec\n" $ diff start end

diff ::Integer -> Integer -> Double
diff start end = fromIntegral (end - start) / (10^12)


getWebSocketR :: Handler ()
getWebSocketR =  do
                              start <- liftIO $ getCPUTime
                              liftIO $ print "before websocket"
                              webSockets {- # SCC "the_websockets" #-} vmxWebSocket
                              liftIO $ print "after websocket"
                              end <- liftIO $ getCPUTime
                              liftIO $ printf "Computation time: %0.3f sec\n" $ diff start end
                              return ()
