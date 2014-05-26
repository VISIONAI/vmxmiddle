{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.WebSocket (getWebSocketR) where

import Import
import Helper.Shared
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


instance FromJSON VMXCommand where
  parseJSON j = do
      o <- {-# SCC "parse_vmxcommand" #-} parseJSON  j
      case H.toList (o :: Object) of
          [("new_connection", Object o')]  -> CreateSession <$> o' .: "model_name"
          [("process_image",   Object o')] -> ProcessImage <$> (o' .: "session_id") <*> (o' .: "image") <*> (o' .: "params") <*> (o' .: "time")
          [("list_sessions",   Object o')] -> return GetSessions
          _                      -> fail "Rule: unexpected format"


data VMXCommand = CreateSession (Maybe String)  
                | GetSessions 
                | ProcessImage SessionId String Value Int
               -- {
               --     piSid'    :: SessionId
               --     picImage' :: String,
               --     processImageParams'   :: Value,
               --     picTime' :: Int
               -- }


vmxWebSocket :: WebSocketsT Handler ()
vmxWebSocket = {-# SCC "dowebsocketforever" #-}(forever $ {-# SCC "insideforever" #-} 
                    do 
                        (d :: B.ByteString) <-                  {-# SCC "receiveData" #-}      receiveData 
                        let (wsc :: Either String VMXCommand) = {-# SCC "decode_websocket" #-} eitherDecode $ L.fromChunks [d]
                        
                        case  wsc of
                            Left error' -> {-# SCC "sendError" #-} sendTextData $ pack error'
                            Right command' -> {-# SCC "parse_command" #-}
                                case command' of
                                    CreateSession model_name -> do
                                        (out :: String) <- lift $ createSession model_name
                                        sendTextData $ pack $ "{\"new_connection\":" <> out <> "}"
                                    ProcessImage sid image params time -> do
                                        (out :: String) <- {-# SCC "processImage" #-} lift $ processImage sid image params time
                                        {-# SCC "pack_output" #-} sendTextData $ pack $"{\"process_image\" : " <> out <> "}"
                            
                        )



getWebSocketR :: Handler ()
getWebSocketR =  do
                              webSockets {- # SCC "the_websockets" #-} vmxWebSocket
                              return ()
