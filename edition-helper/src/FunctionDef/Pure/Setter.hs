{-|
Module : Setter.hs
License : see LICENSE
Description : Setter regroups classes that sets data to primitives
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Pure.Setter
    ( StringLike2Primitive(..)
    , Map2Primitive(..)
    , Tuple2Primitive(..)
    , TupleString2Primitive(..)
    , TupleMap2Primitive(..)
    )
where

import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Map.Strict                ( Map )
import           Utils.MapUtils                 ( convertStringKey
                                                , convertStringVal
                                                , convertTxtMap2String
                                                )
import           Primitive.Definition.ModelInfo ( ModelInfo )
import           Primitive.Definition.ModelData ( ModelData )


class StringLike2Primitive model where
    fromString :: String -> model
    fromText :: Text -> model
    fromText aText = fromString (unpack aText)

class Map2Primitive model where
    fromStringMap :: Map String String -> model
    fromTextMap :: Map Text Text -> model
    fromMixedStrMap :: Map String Text -> model
    fromMixedTextMap :: Map Text String -> model

    fromMixedStrMap aMap = fromTextMap (convertStringKey aMap)
    fromMixedTextMap aMap = fromTextMap (convertStringVal aMap)
    fromTextMap aMap = fromStringMap (convertTxtMap2String aMap)

class Tuple2Primitive model where
    fromTuple :: (ModelInfo, ModelData) -> model

class (StringLike2Primitive model) => TupleString2Primitive model where
    fromTupleString :: (String, String) -> model
    fromTupleText :: (String, Text) -> model
    fromTupleText tpl = fromTupleString (fst tpl, unpack (snd tpl))

class (Map2Primitive model) => TupleMap2Primitive model where
    fromTupleStringMap :: (String, Map String String) -> model
    fromTupleTextMap :: (String, Map Text Text) -> model
    fromTupleMixedStrMap :: (String, Map String Text) -> model
    fromTupleMixedTextMap :: (String, Map Text String) -> model

    fromTupleTextMap tpl = fromTupleStringMap (fst tpl, convertTxtMap2String (snd tpl))
    fromTupleMixedStrMap tpl = fromTupleTextMap (fst tpl, convertStringKey (snd tpl))
    fromTupleMixedTextMap tpl = fromTupleTextMap (fst tpl, convertStringVal (snd tpl))
