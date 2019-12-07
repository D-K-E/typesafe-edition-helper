module MapUtils
    ( add2Map
    , convertTxt2NameMap
    , convertStringKey
    , convertStringVal
    , convertTxtKey
    , convertTxtVal
    , convertStringMap2Txt
    , convertTxtMap2String
    )
where
import           Data.Map                       ( Map )  -- importing type
import qualified Data.Map                      as Dict  -- importing module
import           Data.Text                      ( Text ) -- importing type
import qualified Data.Text                     as Txt  -- importing module

-- |'add2Map' adds [(key, val)] to map by transforming the list to map
-- then making a union with the resulting map
add2Map :: Map Text Text -> [(Text, Text)] -> Map Text Text

add2Map aMap kvs | null kvs  = aMap
                 | otherwise = Dict.union aMap (Dict.fromList kvs)

-- Map functions

-- |'convertTxt2NameMap' converts the key of the map from Data.Text to Xml.Name
convertTxt2NameMap :: Map Text Text -> Map Name Text

convertTxt2NameMap = Dict.mapKeys makeName

-- |'convertStringKey' changes string key of the map to Data.Text
convertStringKey :: Map String Text -> Map Text Text

-- |'convertStringVal' changes string val of the map to Data.Text
convertStringVal :: Map Text String -> Map Text Text

-- |'convertTxtKey' changes Data.Text key of the map to string
convertTxtKey :: Map Text String -> Map String String

-- |'convertTxtVal' changes Data.Text val of the map to string
convertTxtVal :: Map String Text -> Map String String

-- |'convertStringMap2Txt' change string key val of the map to Data.Text
convertStringMap2Txt :: Map String String -> Map Text Text

-- |'convertTxtMap2String' change Data.Text key val of the map to string
convertTxtMap2String :: Map Text Text -> Map String String

convertStringKey = Dict.mapKeys Txt.pack
convertStringVal = Dict.map Txt.pack
convertStringMap2Txt aMap = convertStringVal (convertStringKey aMap)
convertTxtMap2String aMap = convertTxtVal (convertTxtKey aMap)
