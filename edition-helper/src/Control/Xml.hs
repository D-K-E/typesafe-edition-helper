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
import           Primitive.Definition.NodeId   ( NodeId(..) )
import           Primitive.Definition.NodeAttr ( NodeAttr(..) )
import           Primitive.Definition.NodeInfo ( NodeInfo(..) )
import           Primitive.Definition.NodeType ( NodeType(..) )
import           Primitive.Definition.UnitData  ( UnitData(..) )
import           Primitive.Definition.Container
                                               as Cm
                                                ( ContainerModel(..)
                                                , ContainerData(..)
                                                )
import           Primitive.Definition.Error

import           Primitive.Instance.NodeAttr   ( NodeAttr )
import           Primitive.Instance.NodeType   ( NodeType )
import           Primitive.Instance.UnitData    ( UnitData )
-- end def

-- start fn

import           FunctionDef.Transformer        ( Model2StringText(toText)
                                                , Model2Map(toTextMap)
                                                )
import           Control.Pure.NodeId           ( makeNodeIdFromText
                                                , makeNodeIdFromIdTuple
                                                )
import           Control.Pure.NodeAttr         ( makeNodeAttrFromTextMap )
import           Control.Pure.NodeType         ( makeNodeTypeFromText )
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
