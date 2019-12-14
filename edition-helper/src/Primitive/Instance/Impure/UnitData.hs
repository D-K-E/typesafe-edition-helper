{-|
Module : Model
License : see LICENSE
Description : UnitData primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Impure.UnitData
    ( UnitData
    )
where

import           Primitive.Definition.UnitData  ( UnitData
                                                    ( StringUnitDataCons
                                                    , TextUnitDataCons
                                                    )
                                                )
import           Primitive.Instance.Pure.UnitData
                                                ( UnitData )
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                ) -- importing type
import           FunctionDef.Impure.Setter      ( StringLikeSetterM(fromStringM)
                                                )
import           FunctionDef.Impure.Transformer ( Model2StringTextM
                                                    ( toStringM
                                                    , toTextM
                                                    )
                                                )
import           Control.Monad                  ( Monad )

instance StringLikeSetterM UnitData where
    fromStringM aStr | null aStr = fail "empty string is not allowed as data"

instance Model2StringTextM UnitData where
