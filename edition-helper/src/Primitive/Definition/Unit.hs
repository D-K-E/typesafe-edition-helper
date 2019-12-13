{-|
Module : Model
License : see LICENSE
Description : Unit primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Definition.Unit
    ( UnitModel(..)
    )
where

import           Primitive.Definition.ModelInfo ( ModelInfo )
import           Primitive.Definition.UnitData  ( UnitData )

-- | unit model: simple text unit with some meta data
data UnitModel = UnitCons {
    modelInfo :: ModelInfo
  , modelData :: UnitData
  } deriving (Eq, Show)
