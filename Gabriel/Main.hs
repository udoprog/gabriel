module Main where
{-
 - This is Gabriel, the friendly system guardian
 - @author John-John Tedro
 - @license GPLv3 (see LICENSE)
 -}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

import Data.Maybe

import Gabriel.Commands
import Gabriel.Opts

import qualified Gabriel.Concurrent as C
import qualified Gabriel.ProcessState as PS
import qualified Gabriel.Server as S
import qualified Gabriel.Utils as U

import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Daemonize
import System.Posix.Directory
import System.Posix.Process
import System.Posix.Files
import System.Posix.Signals
import System.Posix.Syslog
import System.Process

handleSig :: PS.ProcessState -> S.Server -> IO ()
handleSig state server = do
  PS.writeShutdown state True
  PS.terminateProcess state
  {- signal server, dirty but effective -}
  S.signal server

handlePacket state packet = do
  syslog Notice $ "(unix socket) Got " ++ (show packet)

  res <- (case packet of
    KillCommand -> (do
      PS.writeShutdown state True
      PS.terminateProcess state
      return CommandOk)
    RestartCommand -> (do
      PS.terminateProcess state
      return CommandOk)
    CheckCommand -> do
      ph <- PS.readProcessHandle state
      case ph of
        Just p -> return CommandOk
        Nothing -> return $ CommandError "Process is not running"
    _ -> do
      syslog Notice $ "NOT HANDLED YET"
      return (CommandError "Command not handled"))

  return res

setupProcess :: Options -> [String] -> IO ()
setupProcess opts args = do
  withSyslog ("gabriel[" ++ (processName opts args) ++ "]") [PID, PERROR] USER $ do
    state <- PS.newProcessState
      (optStdout opts)
      (optStderr opts)
      (optRestartInt opts)
      (optCommand opts)

    pid <- getProcessID

    server <- S.server socketPath (\packet -> handlePacket state packet)

    catch (writeFile pidfile (show pid)) (\e -> syslog Error $ "Could not write pid to file - " ++ (show e))
    
    PS.writeProcessCommand state args

    changeWorkingDirectory (optCwd opts)

    installHandler sigTERM (CatchOnce $ handleSig state server) Nothing
    installHandler sigINT  (CatchOnce $ handleSig state server) Nothing

    handle <- mainloop state False

    S.waitFor server

    catch (removeFile socketPath)
      (\e -> syslog Error $ "Could not close and remove socket: " ++ (show e))
    
    catch (removeFile pidfile)
      (\e -> syslog Error $ "Could not remove pid file: " ++ (show e))

  where
    processName :: Options -> [String] -> String
    processName opts args = case (optName opts) of
      Nothing -> U.formatProcessName args " "
      Just n  -> n

    pidfile :: FilePath
    pidfile = fromJust $ optPidfile opts

    socketPath :: FilePath
    socketPath = fromJust $ optSocket opts
    
mainloop :: PS.ProcessState -> Bool -> IO ()
mainloop state True = do
  syslog Notice $ "Shutting Down"
  return ()

mainloop state False = do
  args <- PS.readProcessCommand state

  syslog Notice $ "Running " ++ (U.formatProcessName args " ")

  exitCode <- PS.spawnProcess state

  syslog Notice $ "Process exited " ++ (show exitCode)

  {-
   - Needs to close this here, otherwise pipe will probably be GCed and closed.
   -}
  shutdown <- PS.readShutdown state

  {-
   - This termination was the result of an explicit termination
   -}
  terminate <- PS.takeTerminate state

  case terminate of
    Just _ ->  do
      syslog Debug "Process terminated"
    Nothing -> do
      syslog Debug "Process unexpectedly exited"
 
  delay <- delayTime

  if (not shutdown && delay > 0)
    then (do
      delay' state
      shutdown <- PS.readShutdown state
      mainloop state shutdown)
    else (do
      mainloop state shutdown)

  where
    delayTime = PS.readDelay state

    delay' state = do
      delay <- delayTime
      syslog Notice $ "WAITING " ++ (show delay) ++ " seconds";
      wasInterrupted <- C.controlledThreadDelay 1000000 (PS.readShutdown state) delay
      when (wasInterrupted) (syslog Notice "WAIT was Interrupted")
      return ()

main :: IO ()
main = do
  cmd <- getArgs
  workingDirectory <- getWorkingDirectory

  (opts, args) <- readOptions cmd workingDirectory

  opts <- updateOptions opts

  when (optVerbose opts) (do
    print opts)

  when (optShowVersion opts) (do
    putStrLn "Gabriel, the process guardian version 0.1"
    exitImmediately ExitSuccess)

  let socketPath = fromJust $ optSocket opts

  when (optKill opts) (do
    res <- S.clientPoll socketPath KillCommand
    case res of
      CommandOk -> exitImmediately ExitSuccess
      CommandError msg -> do
        putStrLn $ msg
        exitImmediately $ ExitFailure 1)


  when (optRestart opts) (do
    res <- S.clientPoll socketPath RestartCommand
    case res of
      CommandOk -> exitImmediately ExitSuccess
      CommandError msg -> do
        putStrLn $ msg
        exitImmediately $ ExitFailure 1)

  when (optCheck opts) (do
    res <- S.clientPoll socketPath CheckCommand
    case res of
      CommandOk -> do
        putStrLn $ "Process is OK"
        exitImmediately ExitSuccess
      CommandError msg -> do
        putStrLn $ msg
        exitImmediately $ ExitFailure 1)

  args <- readArgs args (optCommand opts)

  when ((length args) < 1) (do
    ioError $ userError "Too few arguments, requires program after '--'")

  when (optUpdate opts) (do
    let packet = UpdateCommand args
    res <- S.clientPoll socketPath packet
    case res of
      CommandOk -> exitImmediately ExitSuccess
      CommandError msg -> do
        putStrLn $ msg
        exitImmediately $ ExitFailure 1)

  -- sanity checking of the process parameters
  let pidfile = fromJust $ optPidfile opts
  pidfileExists <- doesFileExist pidfile
  when pidfileExists $ ioError (userError $ "Pid file exists " ++ (show pidfile))

  if (optFg opts)
    then (setupProcess opts args)
    else (daemonize $ setupProcess opts args)

  where
    readArgs :: [String] -> Maybe FilePath -> IO [String]
    readArgs args path = do
      if ((length args) < 1)
        then do
          args <- PS.readCommand path
          return args
        else
          return args
        

