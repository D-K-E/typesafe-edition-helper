{-|
Module : Model
License : see LICENSE
Description : XmlRender renders models as xml
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module XmlRenderer where
import           Utils.DataUtils                ( StringLikeCons
                                                , ModelAttrMaker
                                                )
import           Model.ModelId                  ( ModelId(..) )
import           Model.ModelAttr                ( ModelAttr(..) )
import           Model.ModelInfo                ( ModelInfo(..) )
import           Model.ModelType                ( ModelType(..) )
import           Model.Unit                     ( UnitModel(..) )
import           Model.UnitData                 ( UnitData(..) )
import           Model.Container                ( ContainerModel(..) )
import           Model.ContainerData            ( ContainerData(..) )
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import           Text.XML                       ( Node(..)
                                                , Element
                                                , Name
                                                )
import           Utils.MapUtils                 ( add2Map
                                                , convertTxt2NameMap
                                                )
import           Utils.XmlUtils                 ( makeName
                                                , makeTagName
                                                )

-- | transform model to xml node
class Model2XmlNode model where
    toNode :: model -> Node


-- | transform model to xml element
class Model2XmlElement model where
    toElement :: model -> Element

class ModelRenderer model where
    -- function definition
    addIdType2Props :: (model -> ModelInfo) -> model -> Map Txt.Text Txt.Text
    addIdType2Props f mdl = add2Map
        (modelAttrs (f mdl))
        [(Txt.pack "id", modelId (f mdl)),
         (Txt.pack "type", modelType (f mdl))]

    makeElement :: model -> Xml.Element

-- instances
instance Model2XmlNode UnitData where
    toNode (TextUnitDataCons   aMdl) = NodeContent aMdl
    toNode (StringUnitDataCons aMdl) = NodeContent (pack aMdl)

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
