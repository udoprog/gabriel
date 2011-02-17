module Gabriel.Utils( formatProcessName
                    ,
                    ) where
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
