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
    , ModelTuple2Primitive(..)
    , InfoTuple2Primitive(..)
    , TupleString2Primitive(..)
    , TupleMap2Primitive(..)
    )
where

-- start def

-- end def
import           Primitive.Definition.ModelInfo ( ModelInfo )
import           Primitive.Definition.ModelData ( ModelData )
import           Primitive.Definition.ModelId   ( ModelId )
import           Primitive.Definition.ModelType ( ModelType )
import           Primitive.Definition.ModelAttr ( ModelAttr )
import           Primitive.Definition.Error     ( StringValueError
                                                , IdTupleValueError
                                                )
-- start fn

-- end fn

-- start utility
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Map.Strict                ( Map )
import           Utils.MapUtils                 ( convertStringKey
                                                , convertStringVal
                                                , convertTxtMap2String
                                                )

-- end utility

class StringLike2Primitive model where
    fromString :: String -> Either StringValueError model
    fromText :: Text -> Either StringValueError model
    fromText aText = fromString (unpack aText)

class Map2Primitive model where
    fromStringMap :: Map String String -> Either MapValueError model
    fromTextMap :: Map Text Text -> Either MapValueError model
    fromMixedStrMap :: Map String Text -> Either MapValueError model
    fromMixedTextMap :: Map Text String -> Either MapValueError model

    fromMixedStrMap aMap = fromTextMap (convertStringKey aMap)
    fromMixedTextMap aMap = fromTextMap (convertStringVal aMap)
    fromTextMap aMap = fromStringMap (convertTxtMap2String aMap)

class ModelTuple2Primitive model where
    fromModelTuple :: (ModelInfo, ModelData) -> model

class InfoTuple2Primitive model where
    fromInfoTuple :: (ModelId, ModelType, ModelAttr) -> model

class (StringLike2Primitive model) => TupleString2Primitive model where
    fromTupleString :: (String, String) -> Either IdTupleValueError model
    fromTupleText :: (String, Text) -> Either IdTupleValueError model
    fromTupleText tpl = fromTupleString (fst tpl, unpack (snd tpl))

class (Map2Primitive model) => TupleMap2Primitive model where
    fromTupleStringMap :: (String, Map String String) -> Either IdTupleValueError model
    fromTupleTextMap :: (String, Map Text Text) -> Either IdTupleValueError model
    fromTupleMixedStrMap :: (String, Map String Text) -> Either IdTupleValueError model
    fromTupleMixedTextMap :: (String, Map Text String) -> Either IdTupleValueError model

    fromTupleTextMap tpl = fromTupleStringMap (fst tpl, convertTxtMap2String (snd tpl))
    fromTupleMixedStrMap tpl = fromTupleTextMap (fst tpl, convertStringKey (snd tpl))
    fromTupleMixedTextMap tpl = fromTupleTextMap (fst tpl, convertStringVal (snd tpl))
