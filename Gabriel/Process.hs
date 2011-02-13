module Gabriel.Process (setupProcess) where

import Control.Concurrent (threadDelay, forkIO, mergeIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, writeTVar, readTVar, TVar)
import System.IO (writeFile, openFile, hClose, IOMode(..), stdout, stderr, Handle)
import System.Process (waitForProcess, terminateProcess, createProcess, StdStream(..), CreateProcess(..), proc, ProcessHandle)
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import System.Posix.Syslog (syslog, withSyslog, Facility(USER), Priority(Notice, Warning), Option(PID, PERROR))

import Control.Monad (when)
import Data.Maybe (fromJust)

import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigKILL, sigINT, sigTERM, installHandler, Handler(..))

import Gabriel.Opts

handleSig :: Options -> TVar (Maybe ProcessHandle) -> TVar Bool -> IO ()
handleSig opts processVar termVar = do
  atomically $ writeTVar termVar True

  process <- atomically $ readTVar processVar

  case process of
    Nothing -> do
      syslog Notice  "Caught Signal, Nu subprocess running"
    Just p  -> do
      syslog Warning "Caught Signal, Terminating child process"
      terminateProcess p

setupProcess :: Options -> [String] -> FilePath -> IO ()
setupProcess opts args pidfile = withSyslog "gabriel" [PID, PERROR] USER $ do
  termVar     <- atomically $ newTVar False
  processVar  <- atomically $ newTVar Nothing

  pid <- getProcessID

  writeFile pidfile (show pid)

  syslog Notice "Installing Handlers"

  installHandler sigTERM (CatchOnce $ handleSig opts processVar termVar) Nothing
  installHandler sigINT  (CatchOnce $ handleSig opts processVar termVar) Nothing

  runProcess opts args processVar termVar False
  
  removeFile pidfile

runProcess :: Options -> [String] -> TVar (Maybe ProcessHandle) -> TVar Bool -> Bool -> IO ()
runProcess opts args processVar termVar True = do
  syslog Notice $ "Shutting Down"

runProcess opts args processVar termVar False = do
  (out, err) <- setupFiles opts

  syslog Notice $ "STARTING " ++ (show $ head args)

  (_, _, _, p) <- createProcess (proc (head args) (tail args)) {
    std_out = out,
    std_err = err,
    cwd     = optCwd opts
  }

  atomically $ writeTVar processVar (Just p)

  exitCode <- waitForProcess p

  atomically $ writeTVar processVar Nothing

  syslog Notice $ "EXITED " ++ (show exitCode)

  terminate <- atomically $ readTVar termVar

  when (not terminate && delayTime > 0) (delay')

  runProcess opts args processVar termVar terminate

  where
    delayTime = optRestart opts
    delayTimeMs = delayTime * 1000000
    delayShow = show delayTime

    delay' = do
      syslog Notice $ "WAITING " ++ delayShow ++ " seconds";
      threadDelay delayTimeMs

    setupFiles :: Options -> IO (StdStream, StdStream)
    setupFiles opts = do
      outHandle <- case (optStdout opts) of
            Just f  -> do
              handle <- openFile f AppendMode
              return $ UseHandle handle
            Nothing -> return Inherit

      errHandle <- case (optStderr opts) of
            Just f  -> do
              handle <- openFile f AppendMode
              return $ UseHandle handle
            Nothing -> return Inherit

      return (outHandle, errHandle)
