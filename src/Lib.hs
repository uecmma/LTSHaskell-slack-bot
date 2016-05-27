module Lib
    ( someFunc
    ) where

import           Control.Concurrent
import           System.Cron

waiting :: Int -> IO ()
waiting n = do
  threadDelay n
  waiting n

someFunc :: IO ()
someFunc = do
  tids <- execSchedule $ do
    addJob (putStrLn "Job 1") "* * * * *"
    addJob (putStrLn "Job 2") "0 * * * *"
  print tids
  waiting 1000000
