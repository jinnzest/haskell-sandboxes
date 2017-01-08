{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Data.Aeson
import GHC.Generics
import Data.Text as T
import Data.ByteString.Lazy.Char8 as BS

data Person =
  Person { firstName  :: !Text
         , lastName   :: !Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

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
    likesPizza = True
  }

printPerson:: Maybe Person -> IO ()
printPerson person = case person of
  Nothing -> Prelude.putStrLn "nothing"
  Just p -> Prelude.putStrLn $ show p
