module Gabriel.ProcessState
  ( ProcessState
  , SignalStep(..)
  , Signal(..)
  , newProcessState
  , isKilled
  , setKilled
  , readDelay
  , readProcessCommand
  , getHandle
  , setHandle
  , clearHandle
  , isShutdown
  , readStdout
  , readStderr
  , signalProcess
  , killProcess
  , writeDelay
  , writeProcessCommand
  , setShutdown
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad (when)
import Data.Maybe
import System.Exit (ExitCode)
import System.Process
import System.Process.Internals (withProcessHandle_, ProcessHandle__(OpenHandle))
import qualified System.Posix.Signals as S
import qualified System.Posix.Syslog as L
import qualified System.IO as IO

import Gabriel.Utils as U

{-
 - shutdown:        the child and parent process (gabriel) should both terminate
 - terminate:       the child process should terminate
 - processHandle:   the handle for the child process, if this is Nothing, the
 -                  child process is not running
 -}
data ProcessState = ProcessState 
                    { shutdown       :: TVar Bool
                    , killed         :: MVar ()
                    , processHandle  :: TVar (Maybe ProcessHandle)
                    , processCommand :: TVar [String]
                    , stdOut         :: TVar (Maybe FilePath)
                    , stdErr         :: TVar (Maybe FilePath)
                    , commandPath    :: Maybe FilePath
                    , delay          :: TVar Int
                    }

data Signal = KILL | HUP | TERM | NONE deriving (Show, Read)

toSSignal       :: Signal -> S.Signal
toSSignal KILL  = S.sigKILL
toSSignal HUP   = S.sigHUP
toSSignal TERM  = S.sigTERM
toSSignal NONE  = 0

data SignalStep = SignalStep Int Signal deriving (Show)

newProcessState out err delay command = do
  termVar    <- newEmptyMVar
  shutVar    <- atomically $ newTVar False
  processVar <- atomically $ newTVar Nothing
  commandVar <- atomically $ newTVar []
  killReg    <- newEmptyMVar
  outVar     <- atomically $ newTVar out
  errVar     <- atomically $ newTVar err
  delayVar   <- atomically $ newTVar delay
  return ProcessState { shutdown = shutVar
                      , killed  = termVar
                      , processHandle = processVar
                      , processCommand = commandVar
                      , stdOut = outVar
                      , stdErr = errVar
                      , commandPath = command
                      , delay = delayVar
                      }

isKilled        :: ProcessState -> IO Bool
isKilled  state = tryTakeMVar (killed state) >>= return . isJust
setKilled state = tryPutMVar  (killed state) ()

isShutdown        :: ProcessState -> IO Bool
isShutdown  state = atomically $ readTVar $ shutdown state
setShutdown state = atomically $ writeTVar (shutdown state) True

getHandle       :: ProcessState -> IO (Maybe ProcessHandle)
getHandle state = atomically $ readTVar $ processHandle state

setHandle               :: ProcessState -> ProcessHandle -> IO ()
setHandle state process = atomically $ writeTVar (processHandle state) (Just process)

clearHandle       :: ProcessState -> IO ()
clearHandle state = atomically $ writeTVar (processHandle state) Nothing

readProcessCommand state = atomically $ readTVar $ processCommand state
writeProcessCommand state command = do
  U.putArray "command" (commandPath state) command
  atomically $ writeTVar (processCommand state) command

readStdout state = atomically $ readTVar $ stdOut state
readStderr state = atomically $ readTVar $ stdErr state

readDelay state = atomically $ readTVar $ delay state
writeDelay state value = atomically $ writeTVar (delay state) value

internalSignal :: ProcessHandle -> Signal -> IO ()
internalSignal handle (sig) = do
  L.syslog L.Notice $ "Sending " ++ (show sig) ++ " " ++ (show $ toSSignal sig)
  withProcessHandle_ handle (\p -> case p of
    OpenHandle pid -> do
      S.signalProcess (toSSignal sig) pid
      return $ OpenHandle pid)

onPid' :: ProcessState -> (ProcessHandle -> IO a) -> (IO a) -> IO a
onPid' state action nopid = do
  pid <- getHandle state
  case pid of
    Nothing -> nopid
    Just  p -> action p

killProcess :: ProcessState -> [SignalStep] -> IO ()
killProcess state steps =
  onPid' state
    (\p -> sig' p steps)
    (L.syslog L.Notice "No process running")
  where
    {-Just signal the process.-}
    sig' :: ProcessHandle -> [SignalStep] -> IO ()
    sig' _ []    = do
      L.syslog L.Notice "All signals sent"
    sig' p (SignalStep sleep sig:t) = do
      internalSignal p sig
      sig' p t

{-Signal the process signified by ProcessState if it is available.-}
signalProcess :: ProcessState -> [SignalStep] -> IO Bool
signalProcess state steps =
  onPid' state
    (\p -> setKilled state >> kill' p steps >>= return)
    (L.syslog L.Notice "No process running" >> return False)

  where
    delay' seconds = threadDelay (1000000 * seconds)

    {-Signal the process, expecting it to die.-}
    kill' :: ProcessHandle -> [SignalStep] -> IO Bool
    kill' _ [] = do
      L.syslog L.Notice "No more steps to try"
      return False
    kill' p (SignalStep sleep sig:t) = do
      internalSignal p sig

      L.syslog L.Notice $ "Waiting for " ++ (show sleep) ++ " seconds"
      dead <- wait' state sleep

      if dead
        then do
          L.syslog L.Notice $ "Process has been killed"
          return True
        else kill' p t
      where
        wait'             :: ProcessState -> Int -> IO Bool
        wait' _ 0         = return False
        wait' state delay = do
          dead <- isEmptyMVar $ killed state
          L.syslog L.Debug "Sleeping for one second"
          if dead
            then return True
            else delay' 1 >> wait' state (delay - 1)
