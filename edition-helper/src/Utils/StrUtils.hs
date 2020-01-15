module Utils.StrUtils
    ( toLowerStr
    , isAlphaNumStr
    , isAsciiStr
    , toLowerTxt
    , isAlphaNumText
    , isAsciiTxt
    , appendOrPrepend
    , concatHStr
    , concatTStr
    , toTxtList
    )
where


import qualified Data.Char                     as Chr
import           Data.Text                      ( Text
                                                , pack
                                                , toLower
                                                , unpack
                                                )


-- |'concatHStr' concatenates the string to the head of the text
concatHStr :: String -> Text -> Text
concatHStr str txt = pack (str ++ (unpack txt))

-- |'concatTStr' concatenates the string to the tail of the text
concatTStr :: Text -> String -> Text
concatTStr txt str = pack ((unpack txt) ++ str)

-- |'toLowerStr' transform all characters to lower characters
toLowerStr = map Chr.toLower

-- |'toLowerTxt' transform all characters to lower characters for Text
toLowerTxt :: Text -> Text
toLowerTxt = toLower

toTxtList :: [String] -> [Text]
toTxtList strlst = map pack strlst

-- |'isAlphaNumStr' checks if all characters are alphanumeric
isAlphaNumStr :: String -> Bool
isAlphaNumStr = all Chr.isAlphaNum

-- |'isAlphaNumText' checks if all characters are alphanumeric for Text
isAlphaNumText :: Text -> Bool
isAlphaNumText atxt = (isAlphaNumStr . unpack) atxt

-- |'isAsciiStr' checks if all characters are ascii
isAsciiStr :: String -> Bool
isAsciiStr = all Chr.isAscii

isAsciiTxt :: Text -> Bool
isAsciiTxt txt = (isAsciiStr . unpack) txt

-- |'appendOrPrepend' adds str2 either at the start or at the end of str1
appendOrPrepend :: String -> String -> Bool -> String
appendOrPrepend str1 str2 isAppend | isAppend  = str1 ++ "-" ++ str2
                                   | otherwise = str2 ++ "-" ++ str1
