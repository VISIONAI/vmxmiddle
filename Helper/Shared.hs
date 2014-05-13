module Helper.Shared
    ( drainFifo
    , headers
    , getPipeResponse
    , InputPipe
    , OutputPipe
    ) where

import Import
import System.Process
import System.IO
import Control.Exception (evaluate)
import System.FileLock
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson (encode)
import GHC.IO.Handle.FD (openFileBlocking)



-- we use drainFifo instead of a normal readFile because Haskell's non-blocking IO treats FIFOs wrong
drainFifo :: FilePath -> IO String
drainFifo f = do
    (i, out, e, _) <- runInteractiveProcess "bash" ["-c", "cat<"  <>  f] Nothing (Just [])
    hClose i
    hClose e
    hSetBinaryMode out False
    out' <- Control.Exception.evaluate (hGetContents out >>= \x -> length x `seq` return x)
    out'' <- out'
    return out''

headers :: Handler ()
headers = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Content-Type" "application/json"

type InputPipe = FilePath
type OutputPipe = FilePath

getPipeResponse :: Value -> SessionId -> Handler String
getPipeResponse v sid = do
    let f =  LBS.unpack $ encode v
    path <- lockFilePath sid
    i    <- getInputPipe  sid
    o    <- getOutputPipe sid
    ret <- lift $ withFileLock path Shared (\l -> do
        file <- openFileBlocking i WriteMode
        hPutStr file f
        hClose file
        ret' <- drainFifo o
        unlockFile l
        return ret'
        )
    return ret

getInputPipe  sid = fmap (++ "sessions/" ++ sid ++ "/pipe_input")  wwwDir 
getOutputPipe sid = fmap (++ "sessions/" ++ sid ++ "/pipe_output")  wwwDir 
lockFilePath sid = fmap (++ "sessions/" ++ sid ++ "/modelupdate.lock") wwwDir

