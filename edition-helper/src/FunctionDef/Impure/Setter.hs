{-|
Module : Setter.hs
License : see LICENSE
Description : Setter regroups classes that sets data to primitives
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Impure.Setter
    ( StringLikeSetter(..)
    , ModelAttrSetter(..)
    )
where

import qualified FunctionDef.Pure.Setter       as PS
                                                ( StringLikeSetter(..)
                                                , ModelAttrSetter(..)
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


class (StringLikeSetter model) => StringLikeSetterM model where
    fromString :: (MonadFail m) => String -> m model
    fromText :: (MonadFail m) => Text -> m model
    fromString =  return PS.fromString
    fromText aText = fromString (unpack aText)

class (ModelAttrSetter model) => ModelAttrSetterM model where
    fromStringMap :: (MonadFail m) => Map String String -> m model
    fromTextMap :: (MonadFail m) => Map Text Text -> m model
    fromMixedStrMap :: (MonadFail m) => Map String Text -> m model
    fromMixedTextMap :: (MonadFail m) => Map Text String -> m model
    fromStringMap = return PS.fromStringMap

    fromMixedStrMap aMap = fromTextMap (convertStringKey aMap)
    fromMixedTextMap aMap = fromTextMap (convertStringVal aMap)
    fromTextMap aMap = fromStringMap (convertTxtMap2String aMap)
