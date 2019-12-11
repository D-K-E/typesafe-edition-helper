{-|
Module : Model
License : see LICENSE
Description : ModelData.hs is a model for representing all containable data
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Model.ModelData
    ( ModelData(..)
    )
where
import           Model.UnitData                 ( UnitData )
import           Model.Container                ( ContainerData )
import           Model.Container                ( ContainerModel(..) )
import           Utils.ViewUtils                ( Model2Tuple(..)
                                                , Model2StringText(..)
                                                )
import           Utils.ModelUtils               ( StringLikeCons(..)
                                                , ModelAttrMaker(..)
                                                )

data ModelData = UData UnitData
                | CData ContainerData
                deriving (Eq, Show)

instance StringLikeCons ModelData where
    fromString astr = UData (fromString astr)
