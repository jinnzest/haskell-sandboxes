{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Haskell.TH
import Lib

-- $(makeLookupFunctions' ["user", "topic", "post"])
[makeLookupFunctions|user topic post|]

main = do
  userLookup 123 Ok
  topicLookup 456 Warning
  postLookup 789 Error
  printFunc

printFunc = do
  expr <- runQ (makeLookupFun "test")
  putStrLn $ pprint expr
