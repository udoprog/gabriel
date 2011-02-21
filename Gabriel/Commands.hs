module Gabriel.Commands where

import Data.Binary

data Command = UpdateCommand [String] deriving (Show)

instance Binary Command where
    put (UpdateCommand s) = putWord8 0 >> put s
    {-put Coffee = putWord8 1-}
    {-put Tea = putWord8 2-}
    {-put EnergyDrink = putWord8 3-}
    {-put Water = putWord8 4-}
    {-put Wine = putWord8 5-}
    {-put Whisky = putWord8 6-}
    get = do
      tag_ <- getWord8
      case tag_ of
        0 -> get >>= \s -> return (UpdateCommand s)
        {-1 -> return Coffee-}
        {-2 -> return Tea-}
        {-3 -> return EnergyDrink-}
        {-4 -> return Water-}
        {-5 -> return Wine-}
        {-6 -> return Whisky-}

