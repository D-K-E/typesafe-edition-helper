{-|
Module : NodeType
License : see LICENSE
Description : NodeType primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Definition.NodeType
    ( NodeType(..)
    )
where

import           Data.Text                      ( Text ) -- importing type

-- | model type: edition, inflected, glossary it can be constructed from string
data NodeType = TextTypeCons Text
                | StringTypeCons String
                deriving (Eq, Show)
