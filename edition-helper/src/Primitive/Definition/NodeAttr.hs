{-|
Module : Model
License : see LICENSE
Description : NodeAttr primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Definition.NodeAttr
    ( NodeAttr(..)
    )
where

import           Data.Map.Strict                ( Map ) -- importing type
import           Data.Text                      ( Text )

-- | model attribute: unique key value non nested pairs
data NodeAttr = TextAttrCons (Map Text Text)
                | StringAttrCons (Map String String)
                deriving (Eq, Show)
