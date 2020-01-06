{-|
Module : Setter.hs
License : see LICENSE
Description : Setter regroups classes that sets data to primitives
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Setter
    ( Text2NodeIdType(..)
    , Map2Primitive(..)
    , Data2Node(..)
    , IdTuple2Node(..)
    , TupleMap2Primitive(..)
    )
where

-- start def

-- end def
import           Primitive.Definition.Error     ( TextValueError
                                                , IdTupleValueError
                                                , MapValueError
                                                , NodeError
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
                                                , convertStringMap2Txt
                                                )
-- end utility
class Text2NodeIdType model where
    fromText :: Text -> Either TextValueError model

class Data2Node model where
    fromText :: Text -> Either NodeError model
    fromInt :: Int -> Either NodeError model
    fromInteger :: Integer -> Either NodeError model
    fromFloat :: Float -> Either NodeError model
    fromDouble :: Double -> Either NodeError model
    fromBool :: Bool -> Either NodeError model
    fromEmpty :: Nothing -> Either NodeError model
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
    fromTupleEmpty :: (String, Empty) -> Either IdTupleValueError model
    fromTupleContainer :: (String, Container) -> Either IdTupleValueError model
