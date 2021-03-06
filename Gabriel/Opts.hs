module Gabriel.Opts (readOptions, updateOptions, Options(..)) where

import System.Posix.User
import System.Posix.Types
import System.Console.GetOpt
import System.FilePath
import System.Directory
import Data.Maybe (isNothing, fromJust)
import Control.Monad (when)
import Data.List (find)
import Gabriel.ProcessState as PS
import Gabriel.Utils as U

data Options = Options
 { optVerbose    :: Bool
 , optVersion    :: Bool
 , optUpdate     :: Bool
 , optFg         :: Bool
 , optKill       :: Bool
 , optPuts       :: Maybe String
 , optCheck      :: Bool
 , optRestart    :: Bool
 , optCwd        :: FilePath
 , optCommand    :: Maybe FilePath
 , optStdout     :: Maybe FilePath
 , optStderr     :: Maybe FilePath
 , optPidfile    :: Maybe FilePath
 , optSocket     :: Maybe FilePath
 , optName       :: Maybe String
 , optSig        :: Maybe String
 , optUser       :: Maybe String
 , optGroup      :: Maybe String
 , killPattern   :: Maybe [PS.SignalStep]
 , heartBeat     :: Maybe String
 , heartBeatInt  :: Int
 , optRestartInt :: Int
 , optEnviron    :: [(String, String)]
 } deriving Show

defaultOptions wd = Options
 { optVerbose     = False
 , optVersion     = False
 , optUpdate      = False
 , optFg          = False
 , optKill        = False
 , optPuts        = Nothing
 , optCheck       = False
 , optRestart     = False
 , optCwd         = ""
 , optCommand     = Nothing
 , optStdout      = Nothing
 , optStderr      = Nothing
 , optPidfile     = Nothing
 , optSocket      = Nothing
 , optName        = Nothing
 , optSig         = Nothing
 , optUser        = Nothing
 , optGroup       = Nothing
 , killPattern    = Nothing
 , heartBeat      = Nothing
 , heartBeatInt   = 60
 , optRestartInt  = 5
 , optEnviron     = []
 }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['v']     ["verbose"]
     (NoArg (\ opts -> opts { optVerbose = True })) "chatty output on stderr"
 , Option ['V','?'] ["version"]
     (NoArg (\ opts -> opts { optVersion = True })) "show version number"
 , Option ['u'] ["update"]
     (NoArg (\ opts -> opts { optUpdate = True })) "update the running command"
 , Option [] ["kill"]
     (NoArg (\ opts -> opts { optKill = True })) "kill the running command"
 , Option [] ["check"]
     (NoArg (\ opts -> opts { optCheck = True })) "check if the monitor process is running"
 , Option [] ["restart"]
     (NoArg (\ opts -> opts { optRestart = True })) "restart the running command"
 , Option [] ["fg"]
     (NoArg (\ opts -> opts { optFg = True })) "don't daemonize, run in foreground"
 , Option []     ["cwd"]
     (ReqArg (\ f opts -> opts { optCwd = f }) "<dir>")
     "Current working directory"
 , Option []     ["command"]
     (ReqArg (\ f opts -> opts { optCommand = Just f }) "<file>")
     "File to store and/or read running command from depending if arguments are specific or not"
 , Option []     ["stdout"]
     (ReqArg (\ f opts -> opts { optStdout  = Just f }) "<file>")
     "Redirect stdout to FILE"
 , Option []     ["stderr"]
     (ReqArg (\ f opts -> opts { optStderr  = Just f }) "<file>")
     "Redirect stderr to FILE"
 , Option []     ["pidfile"]
     (ReqArg (\ f opts -> opts { optPidfile = Just f }) "<file>")
     "Use FILE as exclusive pidfile"
 , Option ['S']     ["socket"]
     (ReqArg (\ f opts -> opts { optSocket  = Just f }) "<path>")
     "Socket to use for communication"
 , Option []     ["restart-interval"]
     (ReqArg (\ f opts -> opts { optRestartInt = read f }) "<seconds>")
     "Time to wait before restarting the process"
 , Option []     ["name"]
     (ReqArg (\ f opts -> opts { optName = Just f }) "<name>")
     "Name of process to log to syslog"
 , Option []     ["sig"]
     (ReqArg (\ f opts -> opts { optSig = Just f }) "<signal>")
     "Custom signal to send to child process"
 , Option []     ["user"]
     (ReqArg (\ f opts -> opts { optUser = Just f }) "<user>")
     "Run the program as the effective user <user>"
 , Option []     ["group"]
     (ReqArg (\ f opts -> opts { optGroup = Just f }) "<group>")
     "Run the command as the effective group <group>"
 , Option []     ["kill-pattern"]
     (ReqArg (\ f opts -> opts { killPattern = Just $ parseKillPattern f }) "<pattern>")
     "A wait-and-signal pattern, like HUP:10:KILL which will be used to terminate the process"
 , Option [] ["puts"]
     (ReqArg (\ f opts -> opts { optPuts = Just f }) "<string>")
     "Write a <string> to stdin of the running process"
 , Option []     ["heartbeat"]
     (ReqArg (\ f opts -> opts { heartBeat = Just f }) "<command>")
     "Command to execute as heartbeat check, will be run by /bin/sh"
 , Option []     ["heartbeat-interval"]
     (ReqArg (\ f opts -> opts { heartBeatInt = read f }) "<seconds>")
     "Seconds in which to execute heartbeat check"
 , Option ['E'] []
     (ReqArg (\ f opts -> opts { optEnviron = updateEnviron (optEnviron opts) f }) "<name>=<value>")
     "Update environment variable (can be used multiple times)"
 ]

 where
  updateEnviron :: [(String, String)] -> String -> [(String, String)]
  updateEnviron old add = do
    let pos = find (=='=') add
    [("test", "too")]

parseKillPattern :: String -> [PS.SignalStep]
parseKillPattern s = toSignalSteps $ split' s ':'
  where
    split' [] delim = [""]
    split' (c:cs) delim
        | c == delim = "" : rest
        | otherwise = (c : head rest) : tail rest
           where
               rest = split' cs delim
    toSignalSteps :: [String] -> [PS.SignalStep]
    toSignalSteps []           = []
    toSignalSteps [s1]         = PS.SignalStep 0 (U.readM s1 PS.NONE) : toSignalSteps []
    toSignalSteps (s1:s2:rest) = PS.SignalStep (U.readM s2 0) (U.readM s1 PS.NONE) : toSignalSteps rest

updateOptions :: Options -> IO Options
updateOptions opts = do
  wd <- getCurrentDirectory

  let cwd  = joinPath [wd, optCwd opts]
  let cmd  = (canon wd cwd (optCommand opts) "command")
  let out  = (canon wd cwd (optStdout  opts) "out")
  let err  = (canon wd cwd (optStderr  opts) "err")
  let pid  = (canon wd cwd (optPidfile opts) "pid")
  let sock = (canon wd cwd (optSocket  opts) "sock")

  return opts {
    optCwd     = cwd,
    optCommand = Just cmd,
    optStdout  = Just out,
    optStderr  = Just err,
    optPidfile = Just pid,
    optSocket  = Just sock
  }
  where
    oldOrJust val optional = (case val of
      Nothing -> optional
      Just v  -> v)
    canon :: FilePath -> FilePath -> Maybe FilePath -> String -> FilePath
    canon wd _  (Just p) def = joinPath [wd, p]
    canon _  wd Nothing  def = joinPath [wd, def]

readOptions :: [String] -> FilePath -> IO (Options, [String])
readOptions argv workingDirectory = do
  let optsDefinition = options
  let optsDefaults = (defaultOptions workingDirectory)

  (opts, args) <- case getOpt Permute optsDefinition argv of
     (o,n,[]  ) -> return (foldl (flip id) optsDefaults o, n)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header optsDefinition))

  return (opts, args)

  where header = "Usage: gabriel [option...] -- command [command arguments]"
