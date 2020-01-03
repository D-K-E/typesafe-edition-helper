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
    , Container(..)
    )
where

-- start def
import           Primitive.Definition.ModelInfo ( ModelInfo )
-- end def

-- start utilities
import           Data.Text                      ( Text )
-- end utilities

-- | Node data: node that can contain couple of types
data Node = NodeInt Int
    | NodeInteger Integer
    | NodeFloat Float
    | NodeDouble Double
    | NodeString String
    | NodeBool Bool
    | NodeText Text
    | NodeEmpty Nothing
    | NodeContainer Container
    deriving(Eq, Ord, Show)

newtype NodeGroup = NodeGroupCons [Node] deriving (Eq, Ord)

data Container = ContainerCons {
      cinfo :: ModelInfo
    , cdata :: NodeGroup
    } deriving (Eq, Ord)

newtype Graph = GraphCons [NodeGroup] deriving (Eq, Ord)
