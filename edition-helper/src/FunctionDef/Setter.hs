{-|
Module : Setter.hs
License : see LICENSE
Description : Setter regroups classes that sets data to primitives
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Setter
    (Map2Primitive(..)
    , Data2Node(..)
    , IdTuple2Node(..)
    )
where

-- start def

-- end def
import Primitive.Definition.Error
       ( IdTupleValueError, MapValueError, NodeError, TextValueError )
import Primitive.Definition.Node  ( Container, PreNode )

-- start fn

-- end fn

-- start utility
import Data.Map.Strict ( Map )
import Data.Text       ( Text, pack, unpack )
import Utils.MapUtils
       ( convertStringKey
       , convertStringMap2Txt
       , convertStringVal
       , convertTxtMap2String
       )
-- end utility

class Data2Node model where
    fromPreNode :: PreNode -> Either NodeError model
    fromContainer :: Container -> Either NodeError model

class Map2Primitive model where
    fromStringMap :: Map String String -> Either MapValueError model
    fromTextMap :: Map Text Text -> Either MapValueError model
    fromMixedStrMap :: Map String Text -> Either MapValueError model
    fromMixedTextMap :: Map Text String -> Either MapValueError model

    fromMixedStrMap aMap = fromTextMap (convertStringKey aMap)
    fromMixedTextMap aMap = fromTextMap (convertStringVal aMap)
    fromStringMap aMap = fromTextMap (convertStringMap2Txt aMap)

class (Data2Node model) => IdTuple2Node model where
    fromTupleString :: (String, String) -> Either IdTupleValueError model
    fromTupleText :: (String, Text) -> Either IdTupleValueError model
    fromTupleInt :: (String, Int) -> Either IdTupleValueError model
    fromTupleInteger :: (String, Integer) -> Either IdTupleValueError model
    fromTupleFloat :: (String, Float) -> Either IdTupleValueError model
    fromTupleDouble :: (String, Double) -> Either IdTupleValueError model
    fromTupleBool :: (String, Bool) -> Either IdTupleValueError model
    fromTupleEmpty :: (String, Text) -> Either IdTupleValueError model
    fromTupleContainer :: (String, Container) -> Either IdTupleValueError model
