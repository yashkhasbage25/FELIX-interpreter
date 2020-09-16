module FelixError where

import Data.Typeable
import Control.Exception

-- |  FelixError constructor for error
data FelixError = FelixError String deriving (Show, Typeable)

instance Exception FelixError
