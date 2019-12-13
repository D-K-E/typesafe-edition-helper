{-|
Module : Model
License : see LICENSE
Description : UnitData primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.UnitData where

import           Primitive.Definition.UnitData  ( UnitData
                                                , StringUnitDataCons
                                                , TextUnitDataCons
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                ) -- importing type
import           FunctionDef.Setter             ( StringLikeSetter
                                                , fromString
                                                )
import           View.Transformer               ( Model2StringText
                                                , toString
                                                , toText
                                                )

instance StringLikeSetter UnitData where
    fromString aStr | null aStr = error "empty string is not allowed as data"
                    | otherwise = StringUnitDataCons aStr

instance Model2StringText UnitData where
    toString (StringUnitDataCons ud) = ud
    toString (TextUnitDataCons   ud) = unpack ud
    toText (StringUnitDataCons ud) = pack ud
    toText (TextUnitDataCons   ud) = ud
