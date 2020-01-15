module Utils.XmlUtils
    ( makeName
    , makeTagName
    )
where
import Data.Text ( Text, empty, pack )
import Text.XML
       ( Element, Name (..), Node, nameLocalName, nameNamespace, namePrefix )
-- | 'makeName' make an xml name from a text
makeName :: Text -> Name

makeName myText
    | myText == empty = Name { nameLocalName = pack "name"
                             , nameNamespace = Nothing
                             , namePrefix    = Nothing
                             }
    | otherwise = Name { nameLocalName = myText
                       , nameNamespace = Nothing
                       , namePrefix    = Nothing
                       }

-- | 'makeTagName' make a xml tag from a text
makeTagName :: Text -> Text -> Name

makeTagName tagName eltype | empty == tagName = makeName eltype
                           | otherwise        = makeName tagName
