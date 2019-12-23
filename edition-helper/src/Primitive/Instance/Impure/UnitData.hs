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
import           FunctionDef.Pure.Setter        ( StringLike2Primitive
                                                    ( fromString
                                                    )
                                                )
import           FunctionDef.Pure.Transformer   ( Model2StringText
                                                    ( toString
                                                    , toText
                                                    )
                                                )
import           FunctionDef.Impure.Setter      ( StringLike2PrimitiveM
                                                    ( fromStringM
                                                    )
                                                )
import           FunctionDef.Impure.Transformer ( Model2StringTextM
                                                    ( toStringM
                                                    , toTextM
                                                    )
                                                )
import qualified Control.Monad            as Fail
                                                ( fail )

import           Control.Monad             ( Monad )
import           Control.Monad                  ( Monad )

-- end functionality

instance StringLike2PrimitiveM UnitData where
    fromStringM aStr | null aStr =
        Fail.fail "empty string is not allowed as data"

instance Model2StringTextM UnitData where
