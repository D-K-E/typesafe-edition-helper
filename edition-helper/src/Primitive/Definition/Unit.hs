{-|
Module : Model
License : see LICENSE
Description : Unit primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Unit
    ( UnitModel(..)
    )
where

import           Primitive.ModelId              ( ModelId )
import           Primitive.ModelType            ( ModelType )
import           Primitive.ModelAttr            ( ModelAttr )
import           Primitive.ModelInfo            ( ModelInfo )
import           Primitive.UnitData             ( UnitData )
import           Primitive.ModelData            ( ModelData
                                                , UData
                                                )
import           View.Transformer               ( Model2Tuple(..) )

-- | unit model: simple text unit with some meta data
data UnitModel = UnitCons {
    modelInfo :: ModelInfo
  , modelData :: UnitData
  } deriving (Eq, Show)

instance Model2Tuple UnitModel where
    toTuple model = (modelInfo model, UData (modelData model))
