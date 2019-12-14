{-|
Module : Model
License : see LICENSE
Description : ModelData.hs is a model for representing all containable data
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Pure.ModelData
    ( ModelData
    )
where

import           Primitive.Definition.ModelData ( ModelData(UData) )
import           Primitive.Instance.Pure.UnitData
                                                ( UnitData )
import           Control.Pure.UnitData          ( makeUData )
import           FunctionDef.Pure.Setter        ( StringLikeSetter
                                                , fromString
                                                )

instance StringLikeSetter ModelData where
    fromString astr = UData (makeUData astr)
