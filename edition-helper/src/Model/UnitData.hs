{-|
Module : Model
License : see LICENSE
Description : UnitData primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module UnitData
    ( UnitData(..)
    )
where

import           Data.Text                      ( Text ) -- importing type
import           Utils.DataUtils                ( StringLikeCons )

-- | unit data: a string like data
data UnitData = StringUnitDataCons String
                | TextUnitDataCons Text
                deriving (Eq, Show)

instance StringLikeCons UnitData where
    fromString aStr | null aStr = error "empty string is not allowed as data"
                    | otherwise = StringUnitDataCons aStr
