module XmlUtils
    ()
where
import qualified Text.XML                      as Xml
import           Text.XML                       ( Node
                                                , Element
                                                , Name
                                                )
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
