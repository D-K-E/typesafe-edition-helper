{-|
Module : Matcher.hs
License : see LICENSE
Description : Contains typeclasses that are related to matching primitives
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Matcher
    ( MatchModel(..)
    )
where

-- | 'MatchModel' class outlines methods to match model to given field
class MatchModel model where
    -- | 'isSame' checks if the other value is same
    isSame :: model -> model -> Bool

    -- | 'contains' checks if the other value is contained by model
    contains :: model -> model -> Bool
