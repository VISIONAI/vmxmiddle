module Helper.Shared
    ( drainFifo) where

import Import
import System.Process
import System.IO
import Control.Exception (evaluate)

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

