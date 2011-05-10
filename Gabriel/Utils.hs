module Gabriel.Utils( formatProcessName
                    , readM
                    , openFile
                    , openFileM
                    , putArray
                    , getArray
                    ) where

import qualified System.Posix.Syslog as L
import qualified System.IO as IO

{-
 - The utils namespace should contain functions which serve a specific simple
 - purpose.
 -}

formatProcessName :: [String] -> String -> String
formatProcessName array delim = formatProcessName' array delim 0
  where
    formatProcessName' :: [String] -> String -> Int -> String
    formatProcessName' [] _        _ = ""
    formatProcessName' [h]   _     _ = h
    formatProcessName' (h:t) delim 2 = h ++ delim ++ ".."
    formatProcessName' (h:t) delim d = h ++ delim ++ (formatProcessName' t delim (d + 1))

{-
 - Read any Readable type in any Monad.
 - Return the default value @f unless a match can be found.
 -}
readM :: (Read a) => String -> a -> a
readM s f = do
  let m = [x | (x,_) <- reads s]
  case m of
    [x] -> x
    _   -> f

{-Open a handle safely, logging errors and returning Nothing-}
openFile :: String -> IO.IOMode -> String -> IO (Maybe IO.Handle)
openFile name mode path         =
  {- Open the handle, or Nothing if an exception is raised -}
  catch
    (do
      h <- IO.openFile path mode
      return $ Just h)
    (\e -> do
      L.syslog L.Error $ "Failed to open handle '" ++ name ++ "'"
      return Nothing)

openFileM :: String -> IO.IOMode -> Maybe String -> IO (Maybe IO.Handle)
openFileM name mode Nothing     = do
  L.syslog L.Error $ "Failed to open handle '" ++ name ++ "' (Nothing)"
  return Nothing

openFileM name mode (Just path) = openFile name mode path

putArray :: String -> Maybe String -> [String] -> IO ()
putArray name path array = do
  handle <- openFileM "command" IO.WriteMode path
  case handle of
    Just h -> do
      coerce' h array
      IO.hClose h
    Nothing -> return ()
  where
    coerce' h [] = IO.hClose h
    coerce' h [s] = do
      IO.hPutStr h s
      coerce' h []
    coerce' h (s:t) = do
      IO.hPutStr h s
      IO.hPutChar h '\0'
      coerce' h t

getArray :: String -> Maybe FilePath -> IO [String]
getArray name path = do
  handle <- openFileM name IO.ReadMode path
  (case handle of
    Just h -> (do
      coerce' h [] "")
    Nothing -> return [])
  where
    coerce' h a s =
      catch (do
        c <- IO.hGetChar h

        r <- (if (c == '\0')
          then (coerce' h (a ++ [s])  "")
          else (coerce' h a           (s ++ [c])))

        return r)
      (\e -> return $ a ++ [s])
