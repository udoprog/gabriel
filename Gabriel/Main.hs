module Gabriel.Main where
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
import System.Posix.Types
import System.Posix.User
import System.Posix.Signals
import System.Posix.Syslog
import System.Process

defaultKillPattern :: [PS.SignalStep]
defaultKillPattern = [PS.SignalStep 10 PS.TERM, PS.SignalStep 10 PS.KILL]

handleSig :: [PS.SignalStep] -> PS.ProcessState -> S.Server -> IO ()
handleSig pattern state server = do
  PS.setShutdown state
  PS.killProcess state pattern
  {- signal server, dirty but effective -}
  S.signal server

handlePacket :: [PS.SignalStep] -> PS.ProcessState -> Command -> IO Command
handlePacket pattern state packet = do
  syslog Notice $ "(unix socket) Got " ++ (show packet)
  h' packet

  where
    h' KillCommand        = do
      PS.setShutdown state
      PS.killProcess state pattern
      return CommandOk
    h' (SigCommand name)  = do
      let sig = U.readM name PS.NONE
      PS.signalProcess state [PS.SignalStep 0 sig]
      return CommandOk
    h' (Puts str)  = do
      mStdin <- PS.getStdin state
      case mStdin of
        Nothing     -> return $ CommandError "Stdin not registered for process"
        Just stdIn  -> do
          syslog Notice $ "Writing to stdin"
          catch (do
            hPutStrLn stdIn str
            hFlush stdIn)
            (\e -> syslog Error $ "Failed writing to stdin: " ++ (show e))
          return CommandOk
    h' RestartCommand     = do
      PS.killProcess state pattern
      return CommandOk
    h' CheckCommand       = do
      ph <- PS.getHandle state
      case ph of
        Just p -> return CommandOk
        Nothing -> return $ CommandError "Process is not running"
    h' p                  = do
      syslog Notice $ "Command not handled: " ++ (show p)
      return $ CommandError "Command not handled"

process :: Options -> [String] -> IO ()
process opts args = do
  withSyslog ("gabriel[" ++ (processName opts args) ++ "]") [PID, PERROR] DAEMON $ do
    state <- PS.newProcessState
      (optStdout opts)
      (optStderr opts)
      (optRestartInt opts)
      (optCommand opts)

    changeWorkingDirectory (optCwd opts)

    pid    <- getProcessID

    catch (writeFile pidfile (show pid))
      (\e -> syslog Error $ "Could not write pid to file - " ++ (show e))

    PS.writeProcessCommand state args

    server <- S.server socketPath $ handlePacket pattern state

    let signalHandler = CatchOnce $ handleSig pattern state server
    installHandler sigTERM signalHandler Nothing
    installHandler sigINT  signalHandler Nothing

    {-Officially tell the mainloop to handle business as good as it can, no guarantees-}
    forkIO $ mainloop state

    when (isJust $ heartBeat opts) (do
      forkIO $ heartbeatHandle pattern state (fromJust $ heartBeat opts) (heartBeatInt opts)
      return ())
    
    {-Let the socket govern if we should shutdown, S.signal server will also
     - cause a shutdown-}
    S.waitFor server

    catch (removeFile socketPath)
      (\e -> syslog Error $ "Could not close and remove socket: " ++ (show e))
    
    catch (removeFile pidfile)
      (\e -> syslog Error $ "Could not remove pidfile: " ++ (show e))

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

{-
 - Run the heartbeat process, which performs several steps.
 -
 - Run /bin/sh -c "command"
 -
 - If exitCode is Failure, restart the managed process (signal)
 -
 - Execute a delay of heartbeat-interval.
 -
 - If shutdown, end execution. Otherwise continue.
 -}
heartbeatHandle :: [PS.SignalStep] -> PS.ProcessState -> String -> Int ->  IO ()
heartbeatHandle pattern state command sleep = do
  syslog Notice $ "HEARTBEAT: " ++ (show command)

  let args = ["/bin/sh", "-c", command]

  let cp = (proc (head args) (tail args)) {
        std_in  = CreatePipe
      , std_out = Inherit
      , std_err = Inherit }

  (Just s, _, _, p) <- createProcess cp
  hClose s
  exitCode <- waitForProcess p

  case exitCode of
    ExitSuccess   -> return ()
    ExitFailure c -> do
      syslog Notice $ "HEARTBEAT: " ++ (show c)
      PS.killProcess state pattern
      return ()

  C.threadDelay (PS.isShutdown state) sleep

  shutdown <- PS.isShutdown state

  if shutdown
    then return()
    else heartbeatHandle  pattern state command sleep
    
mainloop :: PS.ProcessState -> IO ()
mainloop state = do
  args <- PS.readProcessCommand state

  syslog Notice $ "Running " ++ (U.formatProcessName args " ")

  out <- PS.readStdout state >>= U.openFileM "stdout" AppendMode >>= convert'
  err <- PS.readStderr state >>= U.openFileM "stderr" AppendMode >>= convert'

  let cp = (proc (head args) (tail args)){
        std_in  = CreatePipe
      , std_out = out
      , std_err = err }

  (Just s, _, _, p) <- createProcess cp

  PS.setHandle state p
  PS.setStdin  state s
  exitCode <- waitForProcess p
  PS.clearStdin  state
  PS.clearHandle state

  catch
    (hClose s)
    (\e -> syslog Error $ "Failed to close stdin: " ++ (show e))

  syslog Notice $ "Process exited with code " ++ (show exitCode)

  shutdown <- PS.isShutdown state

  t <- PS.isKilled state

  let msg = if t then "Process Terminated" else "Process unexpectedly terminated"
  
  syslog Debug msg
 
  delay <- PS.readDelay state

  shutdown <-
    if (not shutdown && delay > 0)
      then delay' delay >> PS.isShutdown state
      else return shutdown

  if shutdown
    then return ()
    else mainloop state

  where
    delay' delay = do
      syslog Notice $ "WAITING " ++ (show delay) ++ " seconds";
      wasInterrupted <- C.threadDelay (PS.isShutdown state) delay
      when (wasInterrupted) (syslog Notice "WAIT was Interrupted")
      return ()

    convert' Nothing  = return Inherit
    convert' (Just h) = return $ UseHandle h

main :: IO ()
main = do
  cmd <- getArgs
  workingDirectory <- getWorkingDirectory

  (opts, args) <- readOptions cmd workingDirectory

  opts <- updateOptions opts

  when (optVerbose opts) (do
    print opts)

  when (optVersion opts) (do
    putStrLn "Gabriel, the process guardian version 0.1"
    exitImmediately ExitSuccess)

  let handleCommandS = handleCommand (fromJust $ optSocket opts)

  when (optRestart opts)  $ handleCommandS RestartCommand
  when (optKill opts)     $ handleCommandS KillCommand 
  when (optCheck opts)    $ handleCommandS CheckCommand
  onJust (optSig opts) (\sig -> handleCommandS $ SigCommand sig)
  onJust (optPuts opts) (\string -> handleCommandS $ Puts string)

  args <- readArgs args (optCommand opts)

  when ((length args) < 1) (do
    ioError $ userError "Too few arguments, requires program after '--'")

  when (optUpdate opts) $ handleCommandS $ UpdateCommand args

  {-Set group id-}
  onJust (optGroup opts) (\group -> readGroup group >>= setGroupID)
  {-Set user id-}
  onJust (optUser opts) (\user -> readUser user >>= setUserID)

  {-sanity checking of the process parameters-}
  let pidfile = fromJust $ optPidfile opts
  exists <- doesFileExist pidfile
  when exists $ ioError (userError $ "Pid file exists " ++ (show pidfile))

  let proc = catch (process opts args)
                   (\e -> syslog Error $ "Process Failed: " ++ (show e))

  if (optFg opts)
    then proc
    else daemonize proc

  where
    handleCommand socket command = do
      res <- S.clientPoll socket command
      case res of
        CommandOk -> do
          putStrLn "gabriel: Ok"
          exitImmediately ExitSuccess
        CommandError msg -> do
          putStrLn ("gabriel: Error: " ++ msg)
          exitImmediately $ ExitFailure 1

    {-
     - either use existing arguments, or attempt to read from file.
     -}
    readArgs            :: [String] -> Maybe FilePath -> IO [String]
    readArgs []   path  = U.getArray "command" path >>= return
    readArgs args path  = return args

    onJust                  :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
    onJust (Just j) action  = action j
    onJust Nothing  _       = return ()

    readUser      :: String -> IO UserID
    readUser user = getUserEntryForName user >>= return . userID

    readGroup       :: String -> IO GroupID
    readGroup group = getGroupEntryForName group >>= return . groupID
