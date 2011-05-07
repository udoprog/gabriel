module Gabriel.ProcessState( 
    ProcessState
  , SignalStep(..)
  , Signal(..)
  , newProcessState
  , putTerminate
  , readCommand
  , readDelay
  , readProcessCommand
  , readProcessHandle
  , readShutdown
  , spawnProcess
  , takeTerminate
  , signalProcess
  , writeCommand
  , writeDelay
  , writeProcessCommand
  , writeProcessHandle
  , writeShutdown
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad (when)

import Data.Maybe (isJust, fromJust)

import System.IO
import qualified System.Posix.Signals as S
import qualified System.Posix.Syslog as L
import System.Process (createProcess, proc, CreateProcess(..), StdStream(..), ProcessHandle, waitForProcess)
import System.Process.Internals (withProcessHandle_, ProcessHandle__(OpenHandle))

import Gabriel.Utils as U

{-
 - shutdown:        the child and parent process (gabriel) should both terminate
 - terminate:       the child process should terminate
 - processHandle:   the handle for the child process, if this is Nothing, the
 -                  child process is not running
 -}
data ProcessState = ProcessState 
                    { shutdown            :: TVar Bool
                    , terminate           :: MVar ()
                    , processHandle       :: TVar (Maybe ProcessHandle)
                    , processCommand      :: TVar [String]
                    , stdOut              :: TVar (Maybe FilePath)
                    , stdErr              :: TVar (Maybe FilePath)
                    , commandPath         :: Maybe FilePath
                    , delay               :: TVar Int
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
                      , terminate = termVar
                      , processHandle = processVar
                      , processCommand = commandVar
                      , stdOut = outVar
                      , stdErr = errVar
                      , commandPath = command
                      , delay = delayVar
                      }

takeTerminate state = tryTakeMVar (terminate state)
putTerminate state = tryPutMVar (terminate state) ()

readShutdown state = atomically $ readTVar $ shutdown state
writeShutdown state value = atomically $ writeTVar (shutdown state) value

readProcessHandle state = atomically $ readTVar $ processHandle state
writeProcessHandle state process = atomically $ writeTVar (processHandle state) process

readProcessCommand state = atomically $ readTVar $ processCommand state
writeProcessCommand state command = do
  writeCommand (commandPath state) command
  atomically $ writeTVar (processCommand state) command

writeCommand path command = do
  handle <- safeOpenHandle' "command" WriteMode path
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
  handle <- safeOpenHandle' "command" ReadMode path
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

signalProcess :: Bool -> ProcessState -> [SignalStep] -> IO Bool
signalProcess kill state steps 
  | kill == True = withPid' state (\p -> do
    putTerminate state
    v <- kill' p steps
    return v)
  | otherwise = withPid' state (\p -> do
    sig' p steps
    return True)

  where
    notice' :: String -> IO ()
    notice' msg = L.syslog L.Notice msg

    debug' :: String -> IO ()
    debug' msg = L.syslog L.Debug msg

    delay' seconds = threadDelay (1000000 * seconds)

    withPid' :: ProcessState -> (ProcessHandle -> IO Bool) -> IO Bool
    withPid' state action = do
      pid <- readProcessHandle state

      case pid of
        Nothing -> do
          notice' "No process running"
          return False
        Just  p -> action p

    kill' :: ProcessHandle -> [SignalStep] -> IO Bool
    kill' _ [] = do
      notice' "No more steps to try"
      return False
    kill' p (SignalStep sleep sig:t) = do
      internal' p sig

      notice' $ "Waiting for " ++ (show sleep) ++ " seconds"
      dead <- waitack' state sleep

      if dead
        then do
          notice' $ "Process has been killed"
          return True
        else kill' p t

    sig' :: ProcessHandle -> [SignalStep] -> IO ()
    sig' _ []    = do
      notice' "All signals sent"
    sig' p (SignalStep sleep sig:t) = do
      internal' p sig
      sig' p t

    waitack' :: ProcessState -> Int -> IO Bool
    waitack' _ 0 = return False
    waitack' state delay = do
      dead <- isEmptyMVar $ terminate state

      debug' "Sleeping for one second"

      if dead
        then return True
        else delay' 1 >> waitack' state (delay - 1)

    internal' :: ProcessHandle -> Signal -> IO ()
    internal' handle (sig) = do
      notice' $ "Sending " ++ (show sig) ++ " " ++ (show $ toSSignal sig)
      withProcessHandle_ handle (\p -> case p of
        OpenHandle pid -> do
          S.signalProcess (toSSignal sig) pid
          return $ OpenHandle pid)

spawnProcess state = do
  outPath <- atomically $ readTVar $ stdOut state
  errPath <- atomically $ readTVar $ stdErr state
  args <- readProcessCommand state
  (out, err) <- safeOpenHandles' outPath errPath

  (Just s, _, _, p) <- createProcess (proc (head args) (tail args))
    { std_in  = CreatePipe
    , std_out = out
    , std_err = err
    }

  writeProcessHandle state (Just p)

  exitCode <- waitForProcess p

  writeProcessHandle state Nothing

  {- Important to make sure stdin is not garbage collected (and closed) -}
  hClose s

  return exitCode

  where
    safeOpenHandles' outPath errPath = do
      out <- safeOpenHandle' "stdout" AppendMode outPath
      err <- safeOpenHandle' "stderr" AppendMode errPath
      return (convert' out, convert' err)

      where
        convert' handle = (case handle of Nothing -> Inherit; Just h -> UseHandle h)

{-Open a handle safely, logging errors and returning Nothing-}
safeOpenHandle' name mode (Just path) = 
  {- Open the handle, or Nothing if an exception is raised -}
  catch
    (do
      h <- openFile path mode
      return $ Just h)
    (\e -> do
      L.syslog L.Error $ "Failed to open handle '" ++ name ++ "'"
      return Nothing)

safeOpenHandle' name mode Nothing = do
  L.syslog L.Error $ "Failed to open handle '" ++ name ++ "' (Nothing)"
  return Nothing
