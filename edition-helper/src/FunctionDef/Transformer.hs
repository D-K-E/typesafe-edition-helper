{-|
Module : Transformer
License : see LICENSE
Description : Regroups classes that transforms models into native data types
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Transformer
    ( NodeIdType2Text(..)
    , Model2Map(..)
    , Model2IdTuple(..)
    )
where

-- start def

import FunctionDef.Setter ( Map2Primitive )

-- end def

import Data.Map.Strict ( Map )
import Data.Text       ( Text )

class NodeIdType2Text model where
    toText :: model -> Text

class Model2IdTuple model where
    toIdTuple :: model -> (String, model)

class (Map2Primitive model) => Model2Map model where
    toTextMap :: model -> Map Text Text
