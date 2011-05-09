module Gabriel.Concurrent where

import Control.Concurrent.MVar (tryTakeMVar, MVar)
import qualified Control.Concurrent as C

threadDelay         :: IO Bool -> Int -> IO Bool
threadDelay _    0  = return False
threadDelay lock s  = do
    free <- lock
    if free
      then return True
      else C.threadDelay 1000000 >> threadDelay lock (s - 1)
