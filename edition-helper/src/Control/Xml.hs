{-|
Module : Xml interface to primitives
License : see LICENSE
Description : XML interface to primitives
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module Control.Xml where

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
import           Primitive.Definition.Error

import           Primitive.Instance.ModelAttr   ( ModelAttr )
import           Primitive.Instance.ModelType   ( ModelType )
import           Primitive.Instance.UnitData    ( UnitData )
-- end def

-- start fn

import           FunctionDef.Transformer        ( Model2StringText(toText)
                                                , Model2Map(toTextMap)
                                                )
import           Control.Pure.ModelId           ( makeModelIdFromText
                                                , makeModelIdFromIdTuple
                                                )
import           Control.Pure.ModelAttr         ( makeModelAttrFromTextMap )
import           Control.Pure.ModelType         ( makeModelTypeFromText )
import           Control.Pure.UnitData          ( makeUnitDataFromText
                                                , makeUnitDataFromIdTuple
                                                )
-- end fn

-- start utilities
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
import           Utils.XmlUtils                 ( makeName
                                                , makeTagName
                                                )
import           Utils.MapUtils                 ( add2Map
                                                , convertTxt2NameMap
                                                )
-- end utilities

makeUnitDataFromNode :: Node -> Either StringValueError UnitData
makeUnitDataFromNode (NodeContent txt) = makeUnitDataFromText txt
makeNodeFromUnitData :: UnitData -> Node
makeNodeFromUnitData udata = NodeContent (toText udata)
