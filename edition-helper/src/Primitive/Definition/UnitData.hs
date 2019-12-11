{-|
Module : Model
License : see LICENSE
Description : UnitData primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.UnitData
    ( UnitData(..)
    )
where

import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                ) -- importing type
import           PrimitiveFn.Setter             ( StringLikeSetter(..) )
import           View.Transformer               ( Model2StringText(..) )

-- | unit data: a string like data
data UnitData = StringUnitDataCons String
                | TextUnitDataCons Text
                deriving (Eq, Show)

instance StringLikeSetter UnitData where
    fromString aStr | null aStr = error "empty string is not allowed as data"
                    | otherwise = StringUnitDataCons aStr

instance Model2StringText UnitData where
    toString (StringUnitDataCons ud) = ud
    toString (TextUnitDataCons   ud) = unpack ud
    toText (StringUnitDataCons ud) = pack ud
    toText (TextUnitDataCons   ud) = ud
