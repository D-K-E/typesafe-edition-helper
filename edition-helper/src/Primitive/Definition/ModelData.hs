{-|
Module : Model
License : see LICENSE
Description : ModelData.hs is a model for representing all containable data
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Definition.ModelData
    ( ModelData(..)
    )
where
import           Primitive.Definition.UnitData  ( UnitData )
import           Primitive.Definition.Container ( ContainerData )

data ModelData = UData UnitData
                | CData ContainerData
                deriving (Eq, Show)
