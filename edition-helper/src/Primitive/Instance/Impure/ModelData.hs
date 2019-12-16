{-|
Module : Model
License : see LICENSE
Description : ModelData.hs is a model for representing all containable data
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Impure.ModelData
    ( ModelData
    )
where

import           Primitive.Instance.Pure.ModelData
                                                ( ModelData )

import           Primitive.Instance.Impure.UnitData
                                                ( UnitData )
--import           Control.Impure.UnitData        ( makeUDataM )
import           FunctionDef.Pure.Setter        ( StringLikeSetter(fromString) )
import           FunctionDef.Impure.Setter      ( StringLikeSetterM(fromStringM)
                                                )

instance StringLikeSetterM ModelData where
    fromStringM astr = return (fromString astr)
