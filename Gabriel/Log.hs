module Gabriel.Log(logger, LogLevel(..)) where

import System.IO (Handle, hPutStrLn)

import Control.Monad (forever)

import System.Time (getClockTime, toCalendarTime, CalendarTime(..), formatCalendarTime)
import System.Locale (defaultTimeLocale)

data LogLevel = Debug | Info | Warn | Error deriving (Show, Enum, Eq, Ord)

logger :: Handle -> String -> LogLevel -> String -> IO()
logger handle key level message = do
  tm <- getClockTime
  ct <- toCalendarTime tm
  hPutStrLn handle $ cleanCalendar ct ++ " {" ++ key ++ "} [" ++ levelString ++ "] " ++ message
  where
    levelString = case level of {
      Debug -> "DEBUG";
      Info  -> "INFO ";
      Warn  -> "WARN ";
      Error -> "ERROR";
    }
    cleanCalendar :: CalendarTime -> String
    cleanCalendar ct = formatCalendarTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" ct
