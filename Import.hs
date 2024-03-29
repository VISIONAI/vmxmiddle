module Import
    ( module Import
    ) where

import           Data.Maybe           as Import (fromMaybe)
import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..))

import           Control.Applicative  as Import (pure, (<$>), (<*>))
import           Data.Text            as Import (Text, pack, unpack)

import           Control.Monad        as Import (mzero, when)
import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import
import           System.Process       as Import (readProcess)
import           System.IO            as Import (readFile)
import Data.Aeson                     as Import (eitherDecode, (.:?))
import Control.Concurrent.MVar        as Import 

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))





infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
