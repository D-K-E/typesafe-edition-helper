module Utils.StrUtils
    ( toLowerStr
    , isAlphaNumStr
    , isAsciiStr
    , appendOrPrepend
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

-- |'appendOrPrepend' adds str2 either at the start or at the end of str1
appendOrPrepend :: String -> String -> Bool -> String
appendOrPrepend str1 str2 isAppend | isAppend  = str1 ++ "-" ++ str2
                                   | otherwise = str2 ++ "-" ++ str1
