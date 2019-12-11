{-|
Module : Model
License : see LICENSE
Description : Unit primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Model.Unit
    ( UnitModel(..)
    )
where

import           Model.ModelId                  ( ModelId )
import           Model.ModelType                ( ModelType )
import           Model.ModelAttr                ( ModelAttr )
import           Model.ModelInfo                ( ModelInfo )
import           Model.UnitData                 ( UnitData )
import           Model.ModelData                ( ModelData
                                                , UData
                                                )
import           Utils.ViewUtils                ( Model2Tuple(..) )

-- | unit model: simple text unit with some meta data
data UnitModel = UnitCons {
    modelInfo :: ModelInfo
  , modelData :: UnitData
  } deriving (Eq, Show)

instance Model2Tuple UnitModel where
    toTuple model = (modelInfo model, UData (modelData model))
