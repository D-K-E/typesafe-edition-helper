{-|
Module : Model
License : see LICENSE
Description : ModelData.hs is a model for representing all containable data
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Model.Definition.ModelData
    ( ModelData(..)
    )
where
import           Model.UnitData                 ( UnitData )
import           Model.Container                ( ContainerData )

data ModelData = UData UnitData
                | CData ContainerData
                deriving (Eq, Show)
