{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Main where

import Data.Aeson
import GHC.Generics
import Data.Text as T
import Data.ByteString.Lazy.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as M

data Person =
  Person { firstName  :: !Text
         , lastName   :: !Text
         , age        :: Int
         , likesPizza :: Bool
         , hashMap    :: (Map Text Text)
           } deriving (Show, Generic, ToJSON, FromJSON)

main :: IO ()
main = do
 let str = getPersonJson
 BS.putStrLn str
 printPerson $ decode str

getPersonJson = encode
  Person{
    firstName = "first name",
    lastName = "last name",
    age = 1,
    likesPizza = True,
    hashMap = M.fromList [("1","a"), ("2","b")]
  }

printPerson:: Maybe Person -> IO ()
printPerson person = case person of
  Nothing -> Prelude.putStrLn "nothing"
  Just p -> Prelude.putStrLn $ show p
