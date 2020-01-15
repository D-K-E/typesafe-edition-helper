module Utils.MapUtils
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
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Dict
import           Data.Text       ( Text )
import qualified Data.Text       as Txt
import           Text.XML        ( Name )
import           Utils.XmlUtils  ( makeName )

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
convertStringKey = Dict.mapKeys Txt.pack

-- |'convertStringVal' changes string val of the map to Data.Text
convertStringVal :: Map Text String -> Map Text Text
convertStringVal = Dict.map Txt.pack

-- |'convertTxtKey' changes Data.Text key of the map to string
convertTxtKey :: Map Text String -> Map String String
convertTxtKey = Dict.mapKeys Txt.unpack

-- |'convertTxtVal' changes Data.Text val of the map to string
convertTxtVal :: Map String Text -> Map String String
convertTxtVal = Dict.map Txt.unpack

-- |'convertStringMap2Txt' change string key val of the map to Data.Text
convertStringMap2Txt :: Map String String -> Map Text Text
convertStringMap2Txt aMap = Dict.map Txt.pack (Dict.mapKeys Txt.pack aMap)

-- |'convertTxtMap2String' change Data.Text key val of the map to string
convertTxtMap2String :: Map Text Text -> Map String String
convertTxtMap2String aMap = Dict.map Txt.unpack (Dict.mapKeys Txt.unpack aMap)
