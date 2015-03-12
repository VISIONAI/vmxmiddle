module Helper.Redis 
    ( getRedisR
    , VMXConnection
    , getNextConn
    , setSessionInfo
    )
    where

import Import
import Database.Redis
import Data.ByteString (ByteString)
import Safe (headNote)
import Helper.VMXTypes (CreateModelResponse)
import Data.Aeson (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (encodeUtf8)

type VMXConnection = ByteString

getConnQueue :: Handler (Either Reply [ByteString])
getConnQueue = do
    conn <- getRedisConn
    connections <- liftIO $ runRedis conn $ do
        lrange "connections" 0 (-1) >>= return
    return  connections 

getRedisConn :: Handler Connection
getRedisConn = liftIO $ connect defaultConnectInfo { connectHost = "redishost" } >>= return


setSessionInfo :: SessionId -> CreateModelResponse -> Handler (Either Reply Status)
setSessionInfo sid cmr = do
    conn <- getRedisConn
    liftIO $ runRedis conn $ do
        set (encodeUtf8 sid) (B.concat . BL.toChunks $ encode cmr)

-- right now this actually just grabs the first one in the list
-- lrange will be lpop
    
getNextConn = do
    conn <- getRedisConn
    reply <- liftIO $ runRedis conn $ do
        lrange "connections" 0 (-1) >>= return
    liftIO $ print reply 
    case reply of
        Right connections -> return $ headNote "no available connections"  connections
        Left  bbb -> error $ show bbb
    



getRedisR :: Handler Html
getRedisR = do
    queue <- getConnQueue
    con <- getNextConn
    defaultLayout  [whamlet|#{show queue}<br> #{show con} |]
