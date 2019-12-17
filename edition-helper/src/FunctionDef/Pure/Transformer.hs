{-|
Module : Transformer
License : see LICENSE
Description : Regroups classes that transforms models into native data types
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Pure.Transformer
    ( Model2StringText(..)
    , Model2Tuple(..)
    , Model2Map(..)
    , Model2IdTuple(..)
    )
where

-- start fn
import           FunctionDef.Pure.Setter        ( Map2Primitive
                                                , StringLike2Primitive
                                                , Tuple2Primitive
                                                )
-- end fn

import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           Primitive.Definition.ModelData ( ModelData )
import           Primitive.Definition.ModelInfo ( ModelInfo )


class (StringLike2Primitive model) => Model2StringText model where
    toString :: model -> String
    toText :: model -> Text

class (Tuple2Primitive model) => Model2Tuple model where
    toTuple :: model -> (ModelInfo, ModelData)

class Model2IdTuple model where
    toIdTuple :: model -> (String, model)

class (Map2Primitive model) => Model2Map model where
    toTextMap :: model -> Map Text Text
    toStringMap :: model -> Map String String
