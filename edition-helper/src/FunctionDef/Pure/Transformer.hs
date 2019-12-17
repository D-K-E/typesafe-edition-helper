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
    , ModelInfo2Tuple(..)
    )
where

-- start def

import           FunctionDef.Pure.Setter        ( Map2Primitive
                                                , StringLike2Primitive
                                                , ModelTuple2Primitive
                                                , InfoTuple2Primitive
                                                )
import           Primitive.Definition.ModelData ( ModelData )
import           Primitive.Definition.ModelInfo ( ModelInfo )
import           Primitive.Definition.ModelId   ( ModelId )
import           Primitive.Definition.ModelType ( ModelType )
import           Primitive.Definition.ModelAttr ( ModelAttr )

-- end def

import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )

class (StringLike2Primitive model) => Model2StringText model where
    toString :: model -> String
    toText :: model -> Text

class (ModelTuple2Primitive model) => Model2Tuple model where
    toTuple :: model -> (ModelInfo, ModelData)

class (InfoTuple2Primitive model) => ModelInfo2Tuple model where
    toInfoTuple :: model -> (ModelId, ModelType, ModelAttr)

class Model2IdTuple model where
    toIdTuple :: model -> (String, model)

class (Map2Primitive model) => Model2Map model where
    toTextMap :: model -> Map Text Text
    toStringMap :: model -> Map String String
