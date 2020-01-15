{-|
Module : Node model
License : see LICENSE
Description : Node primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Definition.Node
    ( Node(..)
    , NodeInfo(..)
    , NodeId(..)
    , NodeType(..)
    , NodeAttr(..)
    , PreNode(..)
    , Container(..)
    , NField(..)
    )
where

-- start utilities
import Data.Map.Strict ( Map )
import Data.Text       ( Text, empty, pack )

-- end utilities

-- | node id: alphanumeric non empty string has to be unique for each model
newtype NodeId = TextIdCons Text
                deriving (Eq, Show, Ord)

-- | node type: edition, inflected, glossary it can be constructed from string
newtype NodeType = TextTypeCons Text
                deriving (Eq, Show, Ord)

-- | node attribute: unique key value non nested pairs
newtype NodeAttr = TextAttrCons (Map Text Text)
                deriving (Eq, Show, Ord)

-- | node info: contains meta data with regard to unit/container model
data NodeInfo = InfoCons {
      nodeId   :: NodeId
    , nodeType :: NodeType
    , nodeAttr :: NodeAttr
    } deriving (Eq, Show, Ord)


data PreNode = PreNodeInt Int
    | PreNodeInteger Integer
    | PreNodeFloat Float
    | PreNodeDouble Double
    | PreNodeBool Bool
    | PreNodeText Text
    | PreNodeEmpty Text
    deriving (Eq, Ord, Show)

-- | Node data: node that can contain couple of types
data Node = NodeInt Int
    | NodeInteger Integer
    | NodeFloat Float
    | NodeDouble Double
    | NodeBool Bool
    | NodeText Text
    | NodeEmpty Text
    | NodeContainer Container
    deriving(Eq, Ord, Show)

newtype NodeGroup = NodeGroupCons [Node] deriving (Eq, Ord, Show)

data NField = IdField NodeId
    | TypeField NodeType
    | AttrField NodeAttr
    | NodeField Node
    | NodeInfoField NodeInfo
    | NodeGroupField NodeGroup
    deriving (Eq, Show, Ord)

data Container = ContainerCons {
      cinfo :: NodeInfo
    , cdata :: NodeGroup
    } deriving (Eq, Show, Ord)

newtype Graph = GraphCons [NodeGroup] deriving (Eq, Ord, Show)
