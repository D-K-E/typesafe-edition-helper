{-|
Module : Model
License : see LICENSE
Description : UnitData primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Model.UnitData
    ( UnitData(..)
    )
where

import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                ) -- importing type
import           Utils.ModelUtils               ( StringLikeCons(..) )
import           Utils.ViewUtils                ( Model2StringText(..) )

-- | unit data: a string like data
data UnitData = StringUnitDataCons String
                | TextUnitDataCons Text
                deriving (Eq, Show)

instance StringLikeCons UnitData where
    fromString aStr | null aStr = error "empty string is not allowed as data"
                    | otherwise = StringUnitDataCons aStr

instance Model2StringText UnitData where
    toString (StringUnitDataCons ud) = ud
    toString (TextUnitDataCons   ud) = unpack ud
    toText (StringUnitDataCons ud) = pack ud
    toText (TextUnitDataCons   ud) = ud
