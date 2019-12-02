-- {-# LANGUAGE FlexibleInstances #-}
module XmlRenderer where
-- render container and unit as xml
import           Model                          ( UnitModel
                                                , ContainerData(NestedCons)
                                                , ContainerData(SimpleCons)
                                                , ContainerData
                                                , ContainerModel
                                                , ModelInfo
                                                , modelId
                                                , modelType
                                                , modelAttrs
                                                , unitData
                                                , unitInfo
                                                , containerInfo
                                                , containerData
                                                )
import           Data.Map                       ( Map )  -- importing type
import qualified Data.Map                      as Dict  -- importing module
import qualified Data.Text                      ( Text ) -- importing type
import qualified Data.Text                     as Txt  -- importing module
import qualified Text.XML                       ( Node
                                                , Element
                                                , Name
                                                )
import qualified Text.XML                      as Xml
import           Utils                          ( add2Map
                                                , makeName
                                                , makeTagName
                                                , convertTxt2NameMap
                                                )

class ModelRenderer model where
    -- function definition
    addIdType2Props :: (model -> ModelInfo) -> model -> Map Txt.Text Txt.Text
    addIdType2Props f mdl = add2Map
        (modelAttrs (f mdl))
        [(Txt.pack "id", modelId (f mdl)),
         (Txt.pack "type", modelType (f mdl))]

    makeElement :: model -> Xml.Element


-- transform unit model to xml

instance ModelRenderer UnitModel where
    makeElement um = Xml.Element
        { Xml.elementName       = makeName (Txt.pack "unit")
        , Xml.elementAttributes = convertTxt2NameMap
                                      (addIdType2Props unitInfo um)
        , Xml.elementNodes      = [Xml.NodeContent (unitData um)]
        }


-- transform container model to xml
instance ModelRenderer ContainerModel where
    makeElement cm = Xml.Element
        { Xml.elementName       = makeName (Txt.pack "container")
        , Xml.elementAttributes = convertTxt2NameMap
                                      (addIdType2Props containerInfo cm)
        , Xml.elementNodes      = map (Xml.NodeElement . makeElement)
                                      (containerData cm)
        }

-- transform container data to xml
instance ModelRenderer ContainerData where
    makeElement (NestedCons cm) = makeElement cm
    makeElement (SimpleCons um) = makeElement um
