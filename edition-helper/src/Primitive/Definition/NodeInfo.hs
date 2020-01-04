{-|
Module : Model
License : see LICENSE
Description : NodeInfo primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Definition.NodeInfo
    ( NodeInfo(..)
    )
where

import           Primitive.Definition.NodeId   ( NodeId )
import           Primitive.Definition.NodeType ( NodeType )
import           Primitive.Definition.NodeAttr ( NodeAttr )

-- | model info: contains meta data with regard to unit/container model
data NodeInfo = InfoCons {
      modelId :: NodeId
    , modelType :: NodeType
    , modelAttr :: NodeAttr
    } deriving (Eq, Show)
