module Gabriel.ProcessState( newProcessState
                           , readTerminate
                           , writeTerminate
                           , readProcessHandle
                           , writeProcessHandle
                           , ProcessState
) where

import System.Process (ProcessHandle)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, writeTVar, readTVar, TVar)

data ProcessState = ProcessState 
                    { doTerminate   :: TVar Bool
                    , processHandle :: TVar (Maybe ProcessHandle)
                    }

newProcessState :: IO ProcessState
newProcessState = do
  termVar     <- atomically $ newTVar False
  processVar  <- atomically $ newTVar Nothing
  return ProcessState { doTerminate = termVar, processHandle = processVar}

readTerminate :: ProcessState -> IO Bool
readTerminate state = atomically $ readTVar $ doTerminate state

writeTerminate :: ProcessState -> Bool -> IO ()
writeTerminate state value = atomically $ writeTVar (doTerminate state) value

readProcessHandle :: ProcessState -> IO (Maybe ProcessHandle)
readProcessHandle state = atomically $ readTVar $ processHandle state

writeProcessHandle :: ProcessState -> Maybe ProcessHandle -> IO ()
writeProcessHandle state process = atomically $ writeTVar (processHandle state) process
