{-|
Module : Model
License : see LICENSE
Description : ModelInfo primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Model.Definition.ModelInfo
    ( ModelInfo(..)
    )
where

import           Model.Definition.ModelId       ( ModelId )
import           Model.Definition.ModelType     ( ModelType )
import           Model.Definition.ModelAttr     ( ModelAttr )


-- | model info: contains meta data with regard to unit/container model
data ModelInfo = InfoCons {
      modelId :: ModelId
    , modelType :: ModelType
    , modelAttr :: ModelAttr
    } deriving (Eq, Show)
