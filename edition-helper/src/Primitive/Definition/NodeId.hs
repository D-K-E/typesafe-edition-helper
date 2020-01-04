{-|
Module : NodeId
License : see LICENSE
Description : NodeId primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Definition.NodeId
    ( NodeId(..)
    )
where
import           Data.Text                      ( Text ) -- importing type

-- | model id: alphanumeric non empty string has to be unique for each model
data NodeId = TextIdCons Text
                | StringIdCons String
                deriving (Eq, Show)
