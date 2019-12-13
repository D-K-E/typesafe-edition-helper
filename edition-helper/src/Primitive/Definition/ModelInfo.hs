{-|
Module : Model
License : see LICENSE
Description : ModelInfo primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Definition.ModelInfo
    ( ModelInfo(..)
    )
where

import           Primitive.Definition.ModelId   ( ModelId )
import           Primitive.Definition.ModelType ( ModelType )
import           Primitive.Definition.ModelAttr ( ModelAttr )

-- | model info: contains meta data with regard to unit/container model
data ModelInfo = InfoCons {
      modelId :: ModelId
    , modelType :: ModelType
    , modelAttr :: ModelAttr
    } deriving (Eq, Show)
