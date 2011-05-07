module Gabriel.Commands ( Command(..)
                        ) where

import Data.Binary

data Command = UpdateCommand [String]
             | KillCommand
             | RestartCommand
             | CheckCommand
             | CommandOk
             | CommandError String
             deriving (Show)

instance Binary Command where
    put (UpdateCommand s) = putWord8 0 >> put s
    put (KillCommand) = putWord8 1
    put (RestartCommand) = putWord8 2
    put (CheckCommand) = putWord8 3
    put (CommandOk) = putWord8 128
    put (CommandError s) = putWord8 254 >> put s

    get = do
      tag_ <- getWord8
      case tag_ of
        0 -> get >>= \s -> return (UpdateCommand s)
        1 -> return KillCommand
        2 -> return RestartCommand
        3 -> return CheckCommand
        128 -> return CommandOk
        254 -> get >>= \s -> return (CommandError s)
