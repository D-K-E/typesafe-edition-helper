module Utils
    ( add2Map
    , makeName
    , makeTagName
    , convertTxt2NameMap
    )
where
import           Data.Map                       ( Map )  -- importing type
import qualified Data.Map                      as Dict  -- importing module
import           Data.Text                      ( Text ) -- importing type
import qualified Data.Text                     as Txt  -- importing module
import qualified Text.XML                      as Xml
import           Text.XML                       ( Node
                                                , Element
                                                , Name
                                                )

-- utility functions

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


convertTxt2NameMap :: Map Text Text -> Map Name Text

convertTxt2NameMap = Dict.mapKeys makeName
