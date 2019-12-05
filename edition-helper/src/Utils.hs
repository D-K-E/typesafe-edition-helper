module Utils
    ( add2Map
    , makeName
    , makeTagName
    , convertTxt2NameMap
    , convertStringKey
    , convertStringVal 
    , convertTxtKey
    , convertTxtVal
    , convertStringMap2Txt
    , convertTxtMap2String
    , toLowerStr
    , isAlphaNumStr
    , isAsciiStr
    )
where
import           Data.Map                       ( Map )  -- importing type
import qualified Data.Map                      as Dict  -- importing module
import           Data.Text                      ( Text ) -- importing type
import qualified Data.Text                     as Txt  -- importing module
import qualified Data.Char                     as Chr
import qualified Text.XML                      as Xml
import           Text.XML                       ( Node
                                                , Element
                                                , Name
                                                )

add2Map :: Map Text Text -> [(Text, Text)] -> Map Text Text

add2Map aMap kvs | null kvs  = aMap
                 | otherwise = Dict.union aMap (Dict.fromList kvs)

makeName :: Text -> Xml.Name

makeName myText
    | myText == Txt.empty = Xml.Name { Xml.nameLocalName = Txt.pack "name"
                                     , Xml.nameNamespace = Nothing
                                     , Xml.namePrefix    = Nothing
                                     }
    | otherwise = Xml.Name { Xml.nameLocalName = myText
                           , Xml.nameNamespace = Nothing
                           , Xml.namePrefix    = Nothing
                           }

makeTagName :: Text -> Text -> Name

makeTagName tagName eltype | Txt.empty == tagName = makeName eltype
                           | otherwise            = makeName tagName

-- Map functions

convertTxt2NameMap :: Map Text Text -> Map Name Text

convertTxt2NameMap = Dict.mapKeys makeName

convertStringKey :: Map String Text -> Map Text Text
convertStringVal :: Map Text String -> Map Text Text
convertTxtKey :: Map Text String -> Map String String
convertTxtVal :: Map String Text -> Map String String

convertStringMap2Txt :: Map String String -> Map Text Text
convertTxtMap2String :: Map Text Text -> Map String String

convertStringKey = Dict.mapKeys Txt.pack
convertStringVal = Dict.map Txt.pack
convertStringMap2Txt aMap = convertStringVal (convertStringKey aMap)
convertTxtMap2String aMap = convertTxtVal (convertTxtKey aMap)

-- string functions

-- convert string to lower character
toLowerString = map Chr.toLower

-- alphanumeric check
isAlphaNumStr :: String -> Bool

isAlphaNumString aStr = and (map isAlphaNum)

-- ascii check
isAsciiStr :: String -> Bool

isAsciiString = and (map Chr.isAscii)
