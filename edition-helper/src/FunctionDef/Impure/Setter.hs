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
    fromStringM :: (MonadFail m) => String -> m model
    fromTextM :: (MonadFail m) => Text -> m model
    fromStringM =  return PS.fromString
    fromTextM aText = fromStringM (unpack aText)

class (ModelAttrSetter model) => ModelAttrSetterM model where
    fromStringMapM :: (MonadFail m) => Map String String -> m model
    fromTextMapM :: (MonadFail m) => Map Text Text -> m model
    fromMixedStrMapM :: (MonadFail m) => Map String Text -> m model
    fromMixedTextMapM :: (MonadFail m) => Map Text String -> m model
    fromStringMapM = return PS.fromStringMap

    fromMixedStrMapM aMap = fromTextMapM (convertStringKey aMap)
    fromMixedTextMapM aMap = fromTextMapM (convertStringVal aMap)
    fromTextMapM aMap = fromStringMapM (convertTxtMap2String aMap)
