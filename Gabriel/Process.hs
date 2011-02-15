module Gabriel.Process (setupProcess, checkProcess) where

import Control.Concurrent (threadDelay, forkIO, mergeIO)
import System.IO (writeFile, openFile, hPutStrLn, hGetLine, IOMode(..), stdout, stderr, hClose, Handle)
import System.Process (waitForProcess, terminateProcess, createProcess, StdStream(..), CreateProcess(..), proc)
import System.Directory (removeFile, doesFileExist)
import System.Exit (ExitCode(..))

import Control.Monad (when)
import Data.Maybe (fromJust, isNothing)

import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigKILL, sigINT, sigTERM, sigHUP, installHandler, Handler(..))
import System.Posix.Syslog (syslog, withSyslog, Facility(USER), Priority(Notice, Warning), Option(PID, PERROR))

import Gabriel.Opts
import Gabriel.ProcessState

handleSig :: Options -> ProcessState -> IO ()
handleSig opts state = do
  writeTerminate state True
  handleTerminate state

handleTerminate :: ProcessState -> IO ()
handleTerminate state = do
  process <- readProcessHandle state

  case process of
    Nothing -> do
      syslog Notice "Nu child process running"
    Just p  -> do
      syslog Notice "Terminating child process"
      terminateProcess p

handleHup :: Options -> ProcessState -> IO ()
handleHup opts state = do
  newCommand <- readCommand

  if ((length newCommand) == 0) then do
    syslog Notice $ "Will not reconfigure process to [" ++ (showProcess newCommand " ") ++ "]"
    else do
      writeProcessCommand state newCommand
      syslog Notice $ "Reconfiguring process to [" ++ (showProcess newCommand " ") ++ "]"
      handleTerminate state

  {-process <- readProcessHandle state-}

  {-case process of-}
    {-Nothing -> do-}
      {-syslog Notice "Caught Signal, Nu subprocess running"-}
    {-Just p  -> do-}
      {-syslog Notice "Caught Signal, Terminating child process"-}
      {-terminateProcess p-}
  where
    commandfile = fromJust $ optCommand opts

    readCommand :: IO [String]
    readCommand = do
      handle <- openFile commandfile ReadMode
      readCommand' handle []
      
    readCommand' :: Handle -> [String] -> IO [String]
    readCommand' handle r = do
      line <- catch (do line <- hGetLine handle; return $ Just line) (\e -> return Nothing)

      case line of
        Nothing -> return r
        Just l  -> readCommand' handle (r ++ [l])

{-
 - Check if initialization of the process works correctly
 -
 - This will check that
 -}
checkProcess :: Options -> [String] -> IO ()
checkProcess opts args = do
  pidfileExists <- doesFileExist pidfile
  when pidfileExists $ ioError (userError $ "Pid file exists " ++ (show pidfile))
  where
    pidfile = fromJust $ optPidfile opts

showProcess :: [String] -> String -> String
showProcess array delim = showProcess' array delim 0
  where
    showProcess' :: [String] -> String -> Int -> String
    showProcess' [] _        _ = ""
    showProcess' [h]   _     _ = h
    showProcess' (h:t) delim 2 = h ++ delim ++ ".."
    showProcess' (h:t) delim d = h ++ delim ++ (showProcess' t delim (d + 1))

setupProcess :: Options -> [String] -> IO ()
setupProcess opts args = do
  withSyslog ("gabriel[" ++ processName ++ "]") [PID, PERROR] USER $ do
    state <- newProcessState

    pid <- getProcessID

    writeFile pidfile (show pid)
    writeCommand args
    writeProcessCommand state args

    installHandler sigTERM (CatchOnce $ handleSig opts state) Nothing
    installHandler sigINT  (CatchOnce $ handleSig opts state) Nothing
    installHandler sigHUP  (Catch $ handleHup opts state) Nothing

    handle <- runProcess opts state False
    
    exists <- doesFileExist pidfile
    when exists $ removeFile pidfile

  where
    processName = case (optName opts) of
      Nothing -> showProcess args " "
      Just n  -> n
    pidfile = fromJust $ optPidfile opts

    commandfile = fromJust $ optCommand opts

    writeCommand :: [String] -> IO ()
    writeCommand args = do
      handle <- openFile commandfile WriteMode
      writeCommand' handle args
      
    writeCommand' :: Handle -> [String] -> IO ()
    writeCommand' handle []    = return ()
    writeCommand' handle (h:t) = do
      hPutStrLn handle h
      writeCommand' handle t
    
runProcess :: Options -> ProcessState -> Bool -> IO ()
runProcess opts state True = do
  syslog Notice $ "Shutting Down"
  return ()

runProcess opts state False = do
  (out, err) <- setupFiles opts

  args <- readProcessCommand state

  syslog Notice $ "STARTING " ++ (showProcess args " ")

  (Just pipe, _, _, p) <- createProcess (proc (head args) (tail args)) {
    std_in  = CreatePipe,
    std_out = out,
    std_err = err,
    cwd     = optCwd opts,
    close_fds = False
  }

  writeProcessHandle state (Just p)

  exitCode <- waitForProcess p

  writeProcessHandle state Nothing

  syslog Notice $ "EXITED " ++ (show exitCode)

  {-
   - Needs to close this here, otherwise pipe will probably be GCed and closed.
   -}
  hClose pipe

  terminate <- readTerminate state

  if (not terminate && delayTime > 0) then do
      delay'
      terminate <- readTerminate state
      runProcess opts state terminate
    else do
      runProcess opts state terminate

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
