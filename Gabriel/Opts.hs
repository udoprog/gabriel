module Gabriel.Opts (readOptions, updateOptions, Options(..)) where

import System.Console.GetOpt
import System.FilePath
import System.Directory
import Data.Maybe (isNothing, fromJust)
import Control.Monad (when)
import Data.List (find)

data Options = Options
 { optVerbose     :: Bool
 , optShowVersion :: Bool
 , optUpdate      :: Bool
 , optFg          :: Bool
 , optKill        :: Bool
 , optCheck       :: Bool
 , optRestart     :: Bool
 , optCwd         :: FilePath
 , optCommand     :: Maybe FilePath
 , optStdout      :: Maybe FilePath
 , optStderr      :: Maybe FilePath
 , optPidfile     :: Maybe FilePath
 , optSocket      :: Maybe FilePath
 , optName        :: Maybe String
 , optSig         :: Maybe String
 , optRestartInt  :: Int
 , optEnviron     :: [(String, String)]
 } deriving Show

defaultOptions wd = Options
 { optVerbose     = False
 , optShowVersion = False
 , optUpdate      = False
 , optFg          = False
 , optKill        = False
 , optCheck       = False
 , optRestart     = False
 , optCwd         = wd
 , optCommand     = Nothing
 , optStdout      = Nothing
 , optStderr      = Nothing
 , optPidfile     = Nothing
 , optSocket      = Nothing
 , optName        = Nothing
 , optSig         = Nothing
 , optRestartInt  = 5
 , optEnviron     = []
 }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['v']     ["verbose"]
     (NoArg (\ opts -> opts { optVerbose = True })) "chatty output on stderr"
 , Option ['V','?'] ["version"]
     (NoArg (\ opts -> opts { optShowVersion = True })) "show version number"
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
 , Option ['E'] []
     (ReqArg (\ f opts -> opts { optEnviron = updateEnviron (optEnviron opts) f }) "<name>=<value>")
     "Update environment variable (can be used multiple times)"
 {-, Option ['o']     ["output"]-}
     {-(OptArg ((\ f opts -> opts { optOutput = Just f }) . fromMaybe "output")-}
             {-"FILE")-}
     {-"output FILE"-}
 {-, Option ['c']     []-}
     {-(OptArg ((\ f opts -> opts { optInput = Just f }) . fromMaybe "input")-}
             {-"FILE")-}
     {-"input FILE"-}
 {-, Option ['L']     ["libdir"]-}
     {-(ReqArg (\ d opts -> opts { optLibDirs = optLibDirs opts ++ [d] }) "DIR")-}
     {-"library directory"-}
 ]

 where
  updateEnviron :: [(String, String)] -> String -> [(String, String)]
  updateEnviron old add = do
    let pos = find (=='=') add
    [("test", "too")]

updateOptions :: Options -> IO Options
updateOptions opts = do
  cmd <-  (canon wd (optCommand opts)  "command")
  out  <- (canon wd (optStdout opts)  "out")
  err  <- (canon wd (optStderr opts)  "err")
  pid  <- (canon wd (optPidfile opts) "pid")
  sock <- (canon wd (optSocket opts)  "sock")

  return opts {
    optCommand = Just cmd,
    optStdout  = Just out,
    optStderr  = Just err,
    optPidfile = Just pid,
    optSocket  = Just sock
  }
  where
    wd = optCwd opts
    oldOrJust val optional = (case val of
      Nothing -> optional
      Just v  -> v)
    canon :: FilePath -> Maybe FilePath -> String -> IO FilePath
    canon wd path def = do
      canonicalizePath (case path of
        Just p -> p
        Nothing -> joinPath [wd, def])

readOptions :: [String] -> FilePath -> IO (Options, [String])
readOptions argv workingDirectory = do
  let optsDefinition = options
  let optsDefaults = (defaultOptions workingDirectory)

  (opts, args) <- case getOpt Permute optsDefinition argv of
     (o,n,[]  ) -> return (foldl (flip id) optsDefaults o, n)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header optsDefinition))

  return (opts, args)

  where header = "Usage: gabriel [option...] -- command [command arguments]"
