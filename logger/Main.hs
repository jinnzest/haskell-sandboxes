{-# LANGUAGE OverloadedStrings #-}

module Main
( main
) where

import System.Log.FastLogger
import qualified Data.ByteString.Char8 as BS
import System.IO.Unsafe
import Data.Configurator
import Data.Configurator.Types
import Data.Time.Clock
import Data.Time.ISO8601

data LogLevel = Debug | Info | Warning | Error

instance Show LogLevel where
  show l = case l of
    Debug -> "[DEBUG]"
    Info -> "[INFO]"
    Warning -> "[WARN]"
    Error -> "[ERROR]"

main = writeLog "test log" Debug

{-# NOINLINE globalLogger #-}
globalLogger :: LoggerSet
globalLogger =
  unsafePerformIO $ newFileLoggerSet defaultBufSize "./info.log"

writeLog :: BS.ByteString -> LogLevel -> IO ()
writeLog str logLevel = do
  utcTime <- getCurrentTime
  let timestampStr = formatISO8601Millis utcTime
      timestamp    = BS.pack timestampStr
      logEntry     = timestamp `BS.append` (BS.pack " ") `BS.append` (BS.pack $ show logLevel) `BS.append` (BS.pack " - ") `BS.append` str `BS.append` (BS.pack "\n")
  pushLogStr globalLogger $ toLogStr logEntry