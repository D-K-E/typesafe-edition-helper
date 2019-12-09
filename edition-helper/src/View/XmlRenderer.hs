{-|
Module : Model
License : see LICENSE
Description : XmlRenderer renders models as xml
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module View.XmlRenderer where
import           Model.ModelId                  ( ModelId(..) )
import           Model.ModelAttr                ( ModelAttr(..) )
import           Model.ModelInfo                ( ModelInfo(..) )
import           Model.ModelType                ( ModelType(..) )
import qualified Model.Unit                    as Um
                                                ( UnitModel(..)
                                                , modelInfo
                                                , modelData
                                                )
import           Model.UnitData                 ( UnitData(..) )
import           Model.Container               as Cm
                                                ( ContainerModel(..)
                                                , ContainerData(..)
                                                )
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import           Text.XML                       ( Node(..)
                                                , Element(..)
                                                , Name(..)
                                                , elementName
                                                , elementAttributes
                                                , elementNodes
                                                )
import           Utils.MapUtils                 ( add2Map
                                                , convertTxt2NameMap
                                                )
import qualified Utils.ViewUtils               as VUtils
                                                ( Model2StringText(..)
                                                , Model2Map(..)
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

-- instances
instance Model2XmlNode UnitData where
    toNode (TextUnitDataCons   aMdl) = NodeContent aMdl
    toNode (StringUnitDataCons aMdl) = NodeContent (pack aMdl)

-- transform unit model to xml

instance Model2XmlElement Um.UnitModel where
    toElement um = Element
        { elementName       = makeName (pack "unit")
        , elementAttributes = convertTxt2NameMap
                                  (VUtils.toTxtMap (Um.modelInfo um))
        , elementNodes      = [NodeContent (VUtils.toText (Um.modelData um))]
        }


-- transform container model to xml
instance Model2XmlElement Cm.ContainerModel where
    toElement cm = Element
        { elementName       = makeName (pack "container")
        , elementAttributes = convertTxt2NameMap
                                  (VUtils.toTxtMap (Cm.modelInfo cm))
        , elementNodes      = map (NodeElement . toElement) (Cm.modelData cm)
        }

-- transform container data to xml
instance Model2XmlElement Cm.ContainerData where
    toElement (NestedCons cm) = toElement cm
    toElement (SimpleCons um) = toElement um
