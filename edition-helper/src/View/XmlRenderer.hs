{-|
Module : Model
License : see LICENSE
Description : XmlRenderer renders models as xml
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module View.XmlRenderer where

-- start def
import           Primitive.Definition.ModelId   ( ModelId(..) )
import           Primitive.Definition.ModelAttr ( ModelAttr(..) )
import           Primitive.Definition.ModelInfo ( ModelInfo(..) )
import           Primitive.Definition.ModelType ( ModelType(..) )
import           Primitive.Definition.UnitData  ( UnitData(..) )
import           Primitive.Definition.Container
                                               as Cm
                                                ( ContainerModel(..)
                                                , ContainerData(..)
                                                )

import           Primitive.Instance.Pure.ModelId
                                                ( ModelId(..) )
import           Primitive.Instance.Pure.ModelAttr
                                                ( ModelAttr(..) )
import           Primitive.Instance.Pure.ModelInfo
                                                ( ModelInfo(..) )
import           Primitive.Instance.Pure.ModelType
                                                ( ModelType(..) )
import qualified Primitive.Instance.Pure.Unit  as Um
                                                ( UnitModel(..)
                                                , modelInfo
                                                , modelData
                                                )
import           Primitive.Instance.Pure.UnitData
                                                ( UnitData(..) )
import           Primitive.Instance.Pure.Container
                                               as Cm
                                                ( ContainerModel(..)
                                                , ContainerData(..)
                                                )

-- end def


-- start fn
import qualified FunctionDef.Pure.Transformer  as VUtils
                                                ( Model2StringText(..)
                                                , Model2Map(..)
                                                )


-- end fn

-- start utilites
-- end utilites
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
                                  (VUtils.toTextMap (Um.modelInfo um))
        , elementNodes      = [NodeContent (VUtils.toText (Um.modelData um))]
        }


-- transform container model to xml
instance Model2XmlElement Cm.ContainerModel where
    toElement cm = Element
        { elementName       = makeName (pack "container")
        , elementAttributes = convertTxt2NameMap
                                  (VUtils.toTextMap (Cm.modelInfo cm))
        , elementNodes      = map (NodeElement . toElement) (Cm.modelData cm)
        }

-- transform container data to xml
instance Model2XmlElement Cm.ContainerData where
    toElement (CModel cm) = toElement cm
    toElement (UModel um) = toElement um
