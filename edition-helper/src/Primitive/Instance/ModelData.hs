{-|
Module : Model
License : see LICENSE
Description : ModelData.hs is a model for representing all containable data
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.ModelData
    ( ModelData(..)
    )
where
import           Primitive.UnitData             ( UnitData )
import           Primitive.Container            ( ContainerData )
import           Primitive.Container            ( ContainerModel(..) )
import           View.Transformer               ( Model2Tuple(..)
                                                , Model2StringText(..)
                                                )
import           PrimitiveFn.Setter             ( StringLikeSetter(..)
                                                , ModelAttrMaker(..)
                                                )

data ModelData = UData UnitData
                | CData ContainerData
                deriving (Eq, Show)

instance StringLikeSetter ModelData where
    fromString astr = UData (fromString astr)
