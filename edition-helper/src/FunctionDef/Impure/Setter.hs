{-|
Module : Setter.hs
License : see LICENSE
Description : Setter regroups classes that sets data to primitives
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Impure.Setter
    ( StringLike2PrimitiveM(..)
    , TupleString2PrimitiveM(..)
    , Map2PrimitiveM(..)
    )
where

-- start def

import           Primitive.Definition.ModelInfo ( ModelInfo )
import           Primitive.Definition.ModelData ( ModelData )
import           Primitive.Definition.Error     ( StringValueError(..)
                                                , IdTupleValueError(..)
                                                )
-- end def

import qualified FunctionDef.Pure.Setter       as PS
                                                ( StringLike2Primitive(..)
                                                , TupleString2Primitive(..)
                                                , Map2Primitive(..)
                                                , ModelTuple2Primitive(..)
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Map.Strict                ( Map )
import           Utils.MapUtils                 ( convertStringKey
                                                , convertStringVal
                                                , convertTxtMap2String
                                                )
import           Control.Monad                  ( Monad
                                                , liftM
                                                )


class (PS.StringLike2Primitive model) => StringLike2PrimitiveM model where
    fromStringM :: (Monad m) => String -> m (Either StringValueError model)
    fromTextM :: (Monad m) => Text -> m (Either StringValueError model)
    fromStringM astr = return (Right (PS.fromString astr))
    fromTextM aText = return (Right (PS.fromText aText))

class (PS.TupleString2Primitive model) => TupleString2PrimitiveM model where
    fromTupleStringM :: (Monad m) => (String, String) -> m (Either IdTupleValueError model)
    fromTupleTextM :: (Monad m) => (String, Text) -> m (Either IdTupleValueError model)
    fromTupleTextM tpl = return (Right (PS.fromTupleText tpl))
    fromTupleStringM tpl = return (Right (PS.fromTupleString tpl))

class (PS.Map2Primitive model) => Map2PrimitiveM model where
    fromStringMapM :: (Monad m) => Map String String -> m model
    fromTextMapM :: (Monad m) => Map Text Text -> m model
    fromMixedStrMapM :: (Monad m) => Map String Text -> m model
    fromMixedTextMapM :: (Monad m) => Map Text String -> m model
    fromStringMapM amap = return (PS.fromStringMap amap)

    fromMixedStrMapM aMap = fromTextMapM (convertStringKey aMap)
    fromMixedTextMapM aMap = fromTextMapM (convertStringVal aMap)
    fromTextMapM aMap = fromStringMapM (convertTxtMap2String aMap)
