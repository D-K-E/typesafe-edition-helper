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

-- end def


-- start fn
{-
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
-}
