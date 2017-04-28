{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Logger ( runFileLoggingT )
import Control.Monad.Logger.JSON ( logInfoJ, logDebugJ )
import Data.Aeson.TH ( defaultOptions, deriveJSON )
import Data.Time.Clock ( UTCTime, getCurrentTime )

data Message = Message { time :: UTCTime }

$( deriveJSON defaultOptions ''Message )

main :: IO ()
main =
  runFileLoggingT "info.log"
    (do now <- liftIO getCurrentTime
        $logDebugJ (Message now)
        $logInfoJ "Hello world")
