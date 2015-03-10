module Helper.Redis 
    ( getRedisR
    , VMXConnection
    , getNextConn
    )
    where

import Import
import Database.Redis
import Data.ByteString (ByteString)
import Safe (headNote)

type VMXConnection = ByteString

getConnQueue :: Handler (Either Reply [ByteString])
getConnQueue = do
    conn <- liftIO $ connect defaultConnectInfo { connectHost = "redishost" }
    connections <- liftIO $ runRedis conn $ do
        lrange "connections" 0 (-1) >>= return
    return  connections 


-- right now this actually just grabs the first one in the list
-- lrange will be lpop
    
getNextConn = do
    conn <- liftIO $ connect defaultConnectInfo { connectHost = "redishost" }
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
