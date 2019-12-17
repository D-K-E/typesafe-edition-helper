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
    , Map2PrimitiveM(..)
    , Tuple2PrimitiveM(..)
    )
where

-- start def

import           Primitive.Definition.ModelInfo ( ModelInfo )
import           Primitive.Definition.ModelData ( ModelData )

-- end def

import qualified FunctionDef.Pure.Setter       as PS
                                                ( StringLike2Primitive(..)
                                                , Map2Primitive(..)
                                                , Tuple2Primitive(..)
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
import           Control.Monad.Fail             ( MonadFail )


class (PS.Tuple2Primitive model) => Tuple2PrimitiveM model where
    fromTupleM :: (MonadFail m) => (ModelInfo, ModelData) -> m model
    fromTupleM tpl = return (PS.fromTuple tpl)

class (PS.StringLike2Primitive model) => StringLike2PrimitiveM model where
    fromStringM :: (MonadFail m) => String -> m model
    fromTextM :: (MonadFail m) => Text -> m model
    fromStringM astr =  return (PS.fromString astr)
    fromTextM aText = fromStringM (unpack aText)

class (PS.Map2Primitive model) => Map2PrimitiveM model where
    fromStringMapM :: (MonadFail m) => Map String String -> m model
    fromTextMapM :: (MonadFail m) => Map Text Text -> m model
    fromMixedStrMapM :: (MonadFail m) => Map String Text -> m model
    fromMixedTextMapM :: (MonadFail m) => Map Text String -> m model
    fromStringMapM amap = return (PS.fromStringMap amap)

    fromMixedStrMapM aMap = fromTextMapM (convertStringKey aMap)
    fromMixedTextMapM aMap = fromTextMapM (convertStringVal aMap)
    fromTextMapM aMap = fromStringMapM (convertTxtMap2String aMap)
