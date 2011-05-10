module Gabriel.ProcessState( 
    ProcessState
  , SignalStep(..)
  , Signal(..)
  , newProcessState
  , isKilled
  , setKilled
  , readCommand
  , readDelay
  , readProcessCommand
  , getHandle
  , setHandle
  , clearHandle
  , isShutdown
  , readStdout
  , readStderr
  , signalProcess
  , writeCommand
  , writeDelay
  , writeProcessCommand
  , setShutdown
  , openHandle
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad (when)

import Data.Maybe (isJust, fromJust)

import System.IO
import System.Exit (ExitCode)
import qualified System.Posix.Signals as S
import qualified System.Posix.Syslog as L
import System.Process
import System.Process.Internals (withProcessHandle_, ProcessHandle__(OpenHandle))

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

toSSignal :: Signal -> S.Signal
toSSignal KILL = S.sigKILL
toSSignal HUP  = S.sigHUP
toSSignal TERM = S.sigTERM
toSSignal NONE = 0

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
  writeCommand (commandPath state) command
  atomically $ writeTVar (processCommand state) command

readStdout state = atomically $ readTVar $ stdOut state
readStderr state = atomically $ readTVar $ stdErr state

writeCommand path command = do
  handle <- openHandle "command" WriteMode path
  (case handle of
    Just h -> (do
      coerce' h command
      hClose h)
    Nothing -> return ())
  where
    coerce' h [] = hClose h
    coerce' h [s] = do
      hPutStr h s
      coerce' h []
    coerce' h (s:t) = do
      hPutStr h s
      hPutChar h '\0'
      coerce' h t

readCommand :: Maybe FilePath -> IO [String]
readCommand path = do
  handle <- openHandle "command" ReadMode path
  (case handle of
    Just h -> (do
      coerce' h [] "")
    Nothing -> return [])
  where
    coerce' h a s =
      catch (do
        c <- hGetChar h

        r <- (if (c == '\0')
          then (coerce' h (a ++ [s])  "")
          else (coerce' h a           (s ++ [c])))

        return r)
      (\e -> return $ a ++ [s])

readDelay state = atomically $ readTVar $ delay state
writeDelay state value = atomically $ writeTVar (delay state) value

{-
 - Signal the process signified by ProcessState if it is available.
 -}
signalProcess :: Bool -> ProcessState -> [SignalStep] -> IO Bool
signalProcess kill state steps 
  | kill      = onPid' state (\p -> setKilled state >> kill' p steps >>= return)
  | otherwise = onPid' state (\p -> sig' p steps >> return True)

  where
    notice' :: String -> IO ()
    notice' msg = L.syslog L.Notice msg

    delay' seconds = threadDelay (1000000 * seconds)

    onPid' :: ProcessState -> (ProcessHandle -> IO Bool) -> IO Bool
    onPid' state action = do
      pid <- getHandle state
      case pid of
        Nothing -> notice' "No process running" >> return False
        Just  p -> action p

    {-
     - Signal the process, expecting it to die.
     -}
    kill' :: ProcessHandle -> [SignalStep] -> IO Bool
    kill' _ [] = do
      notice' "No more steps to try"
      return False
    kill' p (SignalStep sleep sig:t) = do
      internal' p sig

      notice' $ "Waiting for " ++ (show sleep) ++ " seconds"
      dead <- waitOrTimeout' state sleep

      if dead
        then do
          notice' $ "Process has been killed"
          return True
        else kill' p t
      where
        waitOrTimeout' :: ProcessState -> Int -> IO Bool
        waitOrTimeout' _ 0 = return False
        waitOrTimeout' state delay = do
          dead <- isEmptyMVar $ killed state
          L.syslog L.Debug "Sleeping for one second"
          if dead
            then return True
            else delay' 1 >> waitOrTimeout' state (delay - 1)

    {-
     - Just signal the process.
     -}
    sig' :: ProcessHandle -> [SignalStep] -> IO ()
    sig' _ []    = do
      notice' "All signals sent"
    sig' p (SignalStep sleep sig:t) = do
      internal' p sig
      sig' p t

    internal' :: ProcessHandle -> Signal -> IO ()
    internal' handle (sig) = do
      notice' $ "Sending " ++ (show sig) ++ " " ++ (show $ toSSignal sig)
      withProcessHandle_ handle (\p -> case p of
        OpenHandle pid -> do
          S.signalProcess (toSSignal sig) pid
          return $ OpenHandle pid)

{-Open a handle safely, logging errors and returning Nothing-}
openHandle name mode (Just path) = 
  {- Open the handle, or Nothing if an exception is raised -}
  catch
    (do
      h <- openFile path mode
      return $ Just h)
    (\e -> do
      L.syslog L.Error $ "Failed to open handle '" ++ name ++ "'"
      return Nothing)

openHandle name mode Nothing = do
  L.syslog L.Error $ "Failed to open handle '" ++ name ++ "' (Nothing)"
  return Nothing
