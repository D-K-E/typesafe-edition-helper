module Utils.StrUtils
    ( toLowerStr
    , isAlphaNumStr
    , isAsciiStr
    )
where


import qualified Data.Char                     as Chr

-- |'toLowerStr' transform all characters to lower characters
toLowerStr = map Chr.toLower

-- |'isAlphaNumStr' checks if all characters are alphanumeric
isAlphaNumStr :: String -> Bool
isAlphaNumStr = all Chr.isAlphaNum

-- |'isAsciiStr' checks if all characters are ascii
isAsciiStr :: String -> Bool
isAsciiStr = all Chr.isAscii
