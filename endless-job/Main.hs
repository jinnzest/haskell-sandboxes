module Main where

import Control.Concurrent.MVar
import System.Cron.Schedule

main :: IO ()
main = do
    done <- newEmptyMVar
    tids <- execSchedule $ do
        addJob job1 "* * * * *"
        addJob job2 "0 * * * *"
    takeMVar done 

job1 :: IO ()
job1 = putStrLn "Job 1"

job2 :: IO ()
job2 = putStrLn "Job 2"
