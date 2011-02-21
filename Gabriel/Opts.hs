module Gabriel.Opts (readOptions, Options(..)) where

import System.Console.GetOpt
import System.FilePath
import Data.Maybe (isNothing)
import Control.Monad (when)
import Data.List (find)

data Options = Options
 { optVerbose     :: Bool
 , optShowVersion :: Bool
 , optUpdate      :: Bool
 , optStdout      :: Maybe FilePath
 , optStderr      :: Maybe FilePath
 , optPidfile     :: Maybe FilePath
 , optCwd         :: Maybe FilePath
 , optCommand     :: Maybe FilePath
 , optSocket      :: Maybe FilePath
 , optName        :: Maybe String
 , optRestart     :: Int
 , optEnviron     :: [(String, String)]
 } deriving Show

defaultOptions wd = Options
 { optVerbose     = False
 , optShowVersion = False
 , optUpdate      = False
 , optStdout      = Just (joinPath [wd, "out"])
 , optStderr      = Just (joinPath [wd, "err"])
 , optPidfile     = Just (joinPath [wd, "pid"])
 , optCommand     = Just (joinPath [wd, "command"])
 , optSocket      = Just (joinPath [wd, "socket"])
 , optCwd         = Just wd
 , optName        = Nothing
 , optRestart     = 5
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
 , Option []     ["stdout"]
     (ReqArg (\ f opts -> opts { optStdout = Just f }) "<file>")
     "Redirect stdout to FILE"
 , Option []     ["stderr"]
     (ReqArg (\ f opts -> opts { optStderr = Just f }) "<file>")
     "Redirect stderr to FILE"
 , Option []     ["pidfile"]
     (ReqArg (\ f opts -> opts { optPidfile = Just f }) "<file>")
     "Use FILE as exclusive pidfile"
 , Option []     ["cwd"]
     (ReqArg (\ f opts -> opts { optCwd = Just f }) "<dir>")
     "Current working directory"
 , Option []     ["command"]
     (ReqArg (\ f opts -> opts { optCommand = Just f }) "<command>")
     "Where to store the running command"
 , Option ['S']     ["socket"]
     (ReqArg (\ f opts -> opts { optSocket = Just f }) "<path>")
     "Socket to use for communication"
 , Option []     ["restart"]
     (ReqArg (\ f opts -> opts { optRestart = read f }) "<seconds>")
     "Time to wait before restarting the process"
 , Option []     ["name"]
     (ReqArg (\ f opts -> opts { optName = Just f }) "<name>")
     "Name of process to log to syslog"
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

readOptions :: [String] -> FilePath -> IO (Options, [String])
readOptions argv workingDirectory = do
  let optsDefinition = options
  let optsDefaults = (defaultOptions workingDirectory)

  (opts, args) <- case getOpt Permute optsDefinition argv of
     (o,n,[]  ) -> return (foldl (flip id) optsDefaults o, n)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header optsDefinition))

  return (opts, args)

  where header = "Usage: gabriel [option...] -- command [command arguments]"
