{-|
Module : Model
License : see LICENSE
Description : ModelAttr primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Definition.ModelAttr
    ( ModelAttr(..)
    )
where

import           Data.Map.Strict                ( Map ) -- importing type
import           Data.Text                      ( Text )

-- | model attribute: unique key value non nested pairs
data ModelAttr = TextAttrCons (Map Text Text)
                | StringAttrCons (Map String String)
                deriving (Eq, Show)
