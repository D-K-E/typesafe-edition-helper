{-|
Module : Transformer
License : see LICENSE
Description : Regroups classes that transforms models into native data types
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module View.Transformer
    ( Model2StringText(..)
    , Model2Tuple(..)
    , Model2Map(..)
    )
where

import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           Primitive.ModelData            ( ModelData )
import           Primitive.ModelInfo            ( ModelInfo )


class Model2StringText model where
    toString :: model -> String
    toText :: model -> Text

class Model2Tuple model where
    toTuple :: model -> (ModelInfo, ModelData)

class Model2Map model where
    toTxtMap :: model -> Map Text Text
    toStringMap :: model -> Map String String
