
module Main where

import Lib
import Control.Monad.Logger (runFileLoggingT)

main = runFileLoggingT "info.log" runService


