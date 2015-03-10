module Helper.Redis 
    ( getRedisR
    , VMXConnection)
    where

import Import
import Database.Redis
import Data.ByteString (ByteString)
import Prelude (head)

type VMXConnection = Text

getConnQueue :: Handler (Either Reply [ByteString])
getConnQueue = do
    conn <- liftIO $ connect defaultConnectInfo { connectHost = "redishost" }
    connections <- liftIO $ runRedis conn $ do
        lrange "connections" 0 (-1) >>= return
    return  connections 

    
getNextConn = do
    conn <- liftIO $ connect defaultConnectInfo { connectHost = "redishost" }
    reply <- liftIO $ runRedis conn $ do
        lrange "connections" 0 (-1) >>= return
    liftIO $ print reply 
    case reply of
        Right aaa -> do
            liftIO $ print aaa
            return $ head aaa
        Left  bbb -> error "???"
    



getRedisR :: Handler Html
getRedisR = do
    queue <- getConnQueue
    con <- getNextConn
    defaultLayout  [whamlet|#{show queue}<br> #{show con} |]
