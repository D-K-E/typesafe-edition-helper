{-|
Module : Setter.hs
License : see LICENSE
Description : Setter regroups classes that sets data to primitives
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Setter
    ( StringLikeSetter(..)
    , ModelAttrSetter(..)
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
import           Control.Monad                  ( Monad )


class StringLikeSetter model where
    fromString :: (Monad m) => String -> m model
    fromText :: (Monad m) => Text -> m model
    fromText aText = fromString (unpack aText)

class ModelAttrSetter model where
    fromStringMap :: (Monad m) => Map String String -> m model
    fromTextMap :: (Monad m) => Map Text Text -> m model
    fromMixedStrMap :: (Monad m) => Map String Text -> m model
    fromMixedTextMap :: (Monad m) => Map Text String -> m model

    fromMixedStrMap aMap = fromTextMap (convertStringKey aMap)
    fromMixedTextMap aMap = fromTextMap (convertStringVal aMap)
    fromTextMap aMap = fromStringMap (convertTxtMap2String aMap)
