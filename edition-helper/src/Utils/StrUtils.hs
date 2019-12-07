module StrUtils
    ( toLowerStr
    , isAlphaNumStr
    , isAsciiStr
    )
where


import qualified Data.Char                     as Chr
-- string functions

-- convert string to lower character
toLowerStr = map Chr.toLower

-- alphanumeric check
isAlphaNumStr :: String -> Bool

isAlphaNumStr aStr = and (map isAlphaNum)

-- ascii check
isAsciiStr :: String -> Bool

isAsciiStr = and (map Chr.isAscii)
