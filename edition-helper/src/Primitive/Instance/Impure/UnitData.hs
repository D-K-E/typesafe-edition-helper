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
import           Primitive.Definition.UnitData  ( UnitData
                                                    ( StringUnitDataCons
                                                    , TextUnitDataCons
                                                    )
                                                )
-- end def
-- start functionality
import           Data.Text                      ( Text
                                                , unpack
                                                , empty
                                                , pack
                                                ) -- importing type
import           FunctionDef.Pure.Setter        ( StringLikeSetter(fromString) )
import           FunctionDef.Pure.Transformer   ( Model2StringText
                                                    ( toString
                                                    , toText
                                                    )
                                                )
import           FunctionDef.Impure.Setter      ( StringLikeSetterM(fromStringM)
                                                )
import           FunctionDef.Impure.Transformer ( Model2StringTextM
                                                    ( toStringM
                                                    , toTextM
                                                    )
                                                )
import qualified Control.Monad.Fail            as Fail
                                                ( fail )

import           Control.Monad.Fail             ( MonadFail )
import           Control.Monad                  ( Monad )

-- end functionality

instance StringLikeSetterM UnitData where
    fromStringM aStr | null aStr =
        Fail.fail "empty string is not allowed as data"

instance Model2StringTextM UnitData where
