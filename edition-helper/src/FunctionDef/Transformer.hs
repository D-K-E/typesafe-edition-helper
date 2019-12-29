{-|
Module : Transformer
License : see LICENSE
Description : Regroups classes that transforms models into native data types
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Transformer
    ( Model2StringText(..)
    , Model2Map(..)
    , Model2IdTuple(..)
    )
where

-- start def

import           FunctionDef.Setter             ( Map2Primitive
                                                , StringLike2Primitive
                                                )

-- end def

import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )

class (StringLike2Primitive model) => Model2StringText model where
    toString :: model -> String
    toText :: model -> Text

class Model2IdTuple model where
    toIdTuple :: model -> (String, model)

class (Map2Primitive model) => Model2Map model where
    toTextMap :: model -> Map Text Text
    toStringMap :: model -> Map String String
