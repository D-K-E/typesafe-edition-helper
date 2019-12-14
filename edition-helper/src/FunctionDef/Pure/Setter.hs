{-|
Module : Setter.hs
License : see LICENSE
Description : Setter regroups classes that sets data to primitives
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Pure.Setter
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


class StringLikeSetter model where
    fromString :: String -> model
    fromText :: Text -> model
    fromText aText = fromString (unpack aText)

class ModelAttrSetter model where
    fromStringMap :: Map String String -> m model
    fromTextMap :: Map Text Text -> m model
    fromMixedStrMap :: Map String Text -> m model
    fromMixedTextMap :: Map Text String -> m model

    fromMixedStrMap aMap = fromTextMap (convertStringKey aMap)
    fromMixedTextMap aMap = fromTextMap (convertStringVal aMap)
    fromTextMap aMap = fromStringMap (convertTxtMap2String aMap)
