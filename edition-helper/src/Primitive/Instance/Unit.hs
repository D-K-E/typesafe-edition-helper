{-|
Module : Model
License : see LICENSE
Description : Unit primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Unit where


import           Primitive.Definition.Unit      ( UnitModel
                                                , modelInfo
                                                , modelData
                                                )
import           Primitive.Definition.ModelData ( ModelData(UData) )
import           View.Transformer               ( Model2Tuple
                                                , toTuple
                                                )

instance Model2Tuple UnitModel where
    toTuple model = (modelInfo model, UData (modelData model))
