{-|
Module : Model
License : see LICENSE
Description : UnitData primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Definition.UnitData
    ( UnitData(..)
    )
where

import           Data.Text                      ( Text ) -- importing type

-- | unit data: a string like data
data UnitData = StringUnitDataCons String
                | TextUnitDataCons Text
                deriving (Eq, Show)
