module Gabriel.Opts (readOptions, Options(..)) where

import System.Console.GetOpt
import System.FilePath

data Options = Options
 { optVerbose     :: Bool
 , optShowVersion :: Bool
 , optStdout      :: Maybe FilePath
 , optStderr      :: Maybe FilePath
 , optPidfile     :: Maybe FilePath
 , optCwd         :: Maybe FilePath
 , optName        :: Maybe String
 , optRestart     :: Int
 } deriving Show

defaultOptions wd = Options
 { optVerbose     = False
 , optShowVersion = False
 , optStdout      = Just (joinPath [wd, "out"])
 , optStderr      = Just (joinPath [wd, "err"])
 , optPidfile     = Just (joinPath [wd, "pid"])
 , optCwd         = Nothing
 , optName        = Nothing
 , optRestart     = 5
 }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['v']     ["verbose"]
     (NoArg (\ opts -> opts { optVerbose = True })) "chatty output on stderr"
 , Option ['V','?'] ["version"]
     (NoArg (\ opts -> opts { optShowVersion = True })) "show version number"
 , Option []     ["stdout"]
     (ReqArg ((\ f opts -> opts { optStdout = Just f })) "<file>")
     "Redirect stdout to FILE"
 , Option []     ["stderr"]
     (ReqArg ((\ f opts -> opts { optStderr = Just f })) "<file>")
     "Redirect stderr to FILE"
 , Option []     ["pidfile"]
     (ReqArg ((\ f opts -> opts { optPidfile = Just f })) "<file>")
     "Use FILE as exclusive pidfile"
 , Option []     ["cwd"]
     (ReqArg ((\ f opts -> opts { optCwd = Just f })) "<dir>")
     "Current working directory"
 , Option []     ["restart"]
     (ReqArg ((\ f opts -> opts { optRestart = read f })) "<seconds>")
     "Time to wait before restarting the process"
 , Option []     ["name"]
     (ReqArg ((\ f opts -> opts { optName = Just f })) "<name>")
     "Name of process to log to syslog"
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

readOptions :: [String] -> FilePath -> IO (Options, [String])
readOptions argv workingDirectory = do
  let optsDefinition = options
  let optsDefaults = (defaultOptions workingDirectory)

  (opts, args) <- case getOpt Permute optsDefinition argv of
     (o,n,[]  ) -> return (foldl (flip id) optsDefaults o, n)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header optsDefinition))

  return (opts, args)

  where header = "Usage: gabriel [option...] -- command [command arguments]"
