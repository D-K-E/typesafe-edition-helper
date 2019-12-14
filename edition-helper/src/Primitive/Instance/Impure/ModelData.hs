{-|
Module : Model
License : see LICENSE
Description : ModelData.hs is a model for representing all containable data
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.ModelData
    ( ModelData
    )
where
import           Primitive.Definition.ModelData ( ModelData(UData) )
import           Primitive.Instance.UnitData    ( UnitData )
import           Control.UnitData               ( makeUData )
import           FunctionDef.Setter             ( StringLikeSetter
                                                , fromString
                                                )

instance StringLikeSetter ModelData where
    fromString astr = return (UData (makeUData astr))
