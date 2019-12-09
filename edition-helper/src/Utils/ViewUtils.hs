{-|
Module : ViewUtils
License : see LICENSE
Description : Contains typeclasses that are common for view related aspects
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module Utils.ViewUtils
    ( Model2StringText(..)
    , Model2Map(..)
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

class Model2StringText model where
    toString :: model -> String
    toText :: model -> Text

class Model2Map model where
    toTxtMap :: model -> Map Text Text
    toStringMap :: model -> Map String String
