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
import Primitive.Definition.Container as Cm
       ( ContainerData (..), ContainerModel (..) )
import Primitive.Definition.Error
import Primitive.Definition.NodeAttr  ( NodeAttr (..) )
import Primitive.Definition.NodeId    ( NodeId (..) )
import Primitive.Definition.NodeInfo  ( NodeInfo (..) )
import Primitive.Definition.NodeType  ( NodeType (..) )
import Primitive.Definition.UnitData  ( UnitData (..) )

import Primitive.Instance.NodeAttr ( NodeAttr )
import Primitive.Instance.NodeType ( NodeType )
import Primitive.Instance.UnitData ( UnitData )
-- end def

-- start fn

import Control.Pure.NodeAttr   ( makeNodeAttrFromTextMap )
import Control.Pure.NodeId     ( makeNodeIdFromIdTuple, makeNodeIdFromText )
import Control.Pure.NodeType   ( makeNodeTypeFromText )
import Control.Pure.UnitData
       ( makeUnitDataFromIdTuple, makeUnitDataFromText )
import FunctionDef.Transformer
       ( Model2Map (toTextMap), NodeIdType2Text (toText) )
-- end fn

-- start utilities
import Data.Map.Strict ( Map )
import Data.Text       ( Text, pack, unpack )
import Text.XML
       ( Element (..)
       , Name (..)
       , Node (..)
       , elementAttributes
       , elementName
       , elementNodes
       )
import Utils.MapUtils  ( add2Map, convertTxt2NameMap )
import Utils.XmlUtils  ( makeName, makeTagName )
-- end utilities

makeUnitDataFromNode :: Node -> Either TextValueError UnitData
makeUnitDataFromNode (NodeContent txt) = makeUnitDataFromText txt
makeNodeFromUnitData :: UnitData -> Node
makeNodeFromUnitData udata = NodeContent (toText udata)
