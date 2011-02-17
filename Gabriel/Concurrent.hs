module Gabriel.Concurrent where

import Control.Concurrent.MVar (tryTakeMVar, MVar)
import Control.Concurrent (threadDelay)

controlledThreadDelay :: Int -> (IO Bool) -> Int -> IO Bool
controlledThreadDelay _     _    0     = return False
controlledThreadDelay sleep lock limit = do
  free <- lock
  if free then return True else (do
    threadDelay sleep
    controlledThreadDelay sleep lock (limit - 1))
