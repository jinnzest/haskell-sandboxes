{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Configurator
import Data.Configurator.Types (Value, Configured, convert)

main = do
  cfg <- load [Required "my.cfg"]
  num <- require cfg "parent-group.num" :: IO Int
  putStrLn $ "int value: " ++ show (num*2)
  str <- require cfg "parent-group.sub-group.str" :: IO String
  putStrLn $ "string value: " ++ str
