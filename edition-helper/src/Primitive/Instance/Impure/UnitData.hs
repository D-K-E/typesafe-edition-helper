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

-- start def
import           Primitive.Definition.UnitData  ( UnitData
                                                    ( StringUnitDataCons
                                                    , TextUnitDataCons
                                                    )
                                                )
import           Primitive.Instance.Pure.UnitData
                                                ( UnitData )
-- end def
-- start functionality
import           Data.Text                      ( Text
                                                , unpack
                                                , empty
                                                , pack
                                                ) -- importing type
import           FunctionDef.Impure.Setter      ( StringLikeSetterM(fromStringM)
                                                )
import           FunctionDef.Impure.Transformer ( Model2StringTextM
                                                    ( toStringM
                                                    , toTextM
                                                    )
                                                )
import           Control.Monad.Fail             ( MonadFail )
-- end functionality

instance StringLikeSetterM UnitData where
    fromStringM aStr | null aStr = fail "empty string is not allowed as data"

instance Model2StringTextM UnitData where
    toStringM (StringUnitDataCons astr) | null astr = fail "empty unit data"
    toStringM (TextUnitDataCons astr) | empty astr  = fail "empty unit data"
    toTextM (TextUnitDataCons astr) | empty astr  = fail "empty unit data"
    toTextM (StringUnitDataCons astr) | null astr = fail "empty unit data"
