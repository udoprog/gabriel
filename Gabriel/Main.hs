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

defaultKillPattern :: [PS.SignalStep]
defaultKillPattern = [PS.SignalStep 10 PS.TERM, PS.SignalStep 10 PS.KILL]

handleSig :: [PS.SignalStep] -> PS.ProcessState -> S.Server -> IO ()
handleSig pattern state server = do
  PS.writeShutdown state True
  PS.signalProcess True state pattern
  {- signal server, dirty but effective -}
  S.signal server

handlePacket :: [PS.SignalStep] -> PS.ProcessState -> Command -> IO Command
handlePacket pattern state packet = do
  syslog Notice $ "(unix socket) Got " ++ (show packet)

  res <- (case packet of
    KillCommand -> (do
      PS.writeShutdown state True
      PS.signalProcess True state pattern
      return CommandOk)
    SigCommand name -> (do
      let sig = U.readM name PS.NONE
      PS.signalProcess False state [PS.SignalStep 0 sig]
      return CommandOk)
    RestartCommand -> (do
      PS.signalProcess True state pattern
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

    let server = handlePacket pattern state

    pid    <- getProcessID
    server <- S.server socketPath server

    let sigHandler = handleSig pattern state server

    catch (writeFile pidfile (show pid)) (\e -> syslog Error $ "Could not write pid to file - " ++ (show e))
    
    PS.writeProcessCommand state args

    changeWorkingDirectory (optCwd opts)

    installHandler sigTERM (CatchOnce $ sigHandler) Nothing
    installHandler sigINT  (CatchOnce $ sigHandler) Nothing

    {-Officially tell the mainloop to handle business as good as it can, no guarantees-}
    forkIO $ mainloop state

    {-Let the socket govern if we should shutdown, S.signal server will also
     - cause a shutdown-}
    S.waitFor server

    catch (removeFile socketPath)
      (\e -> syslog Error $ "Could not close and remove socket: " ++ (show e))
    
    catch (removeFile pidfile)
      (\e -> syslog Error $ "Could not remove pid file: " ++ (show e))

  where
    pattern :: [PS.SignalStep]
    pattern = (case (killPattern opts) of
        Just p -> p
        Nothing -> defaultKillPattern)

    processName :: Options -> [String] -> String
    processName opts args = case (optName opts) of
      Nothing -> U.formatProcessName args " "
      Just n  -> n

    pidfile :: FilePath
    pidfile = fromJust $ optPidfile opts

    socketPath :: FilePath
    socketPath = fromJust $ optSocket opts
    
mainloop :: PS.ProcessState -> IO ()
mainloop state = do
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

  shutdown <-
    if (not shutdown && delay > 0)
      then (do
        delay' state
        PS.readShutdown state)
      else
        return shutdown

  if shutdown
    then return ()
    else mainloop state

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

  when (isJust $ optSig opts) (do
    res <- S.clientPoll socketPath (SigCommand $ fromJust $ optSig opts)
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
        

