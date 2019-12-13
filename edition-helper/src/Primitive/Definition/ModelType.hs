{-|
Module : ModelType
License : see LICENSE
Description : ModelType primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Definition.ModelType
    ( ModelType(..)
    )
where

import           Data.Text                      ( Text ) -- importing type

-- | model type: edition, inflected, glossary it can be constructed from string
data ModelType = TextTypeCons Text
                | StringTypeCons String
                deriving (Eq, Show)
