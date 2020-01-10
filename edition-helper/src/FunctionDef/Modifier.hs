{-|
Module : Modifier.hs
License : see LICENSE
Description : Contains typeclasses that are related to modifying primitives
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Modifier
    ( ReplaceInfoField(..)
    , ReplaceField(..)
    , Add2Field(..)
    )
where

import Primitive.Definition.Node (NField)

-- | 'ReplaceField' class outlines methods to replace model info fields
class ReplaceField model where

    -- |'replaceField' replaces a field of model
    replaceField :: model -> NField -> model

-- | 'Add2Field' class outlines methods to add new value to given field
class (ReplaceField model) => Add2Field model where
    insertField :: model -> NField -> Bool -> model
