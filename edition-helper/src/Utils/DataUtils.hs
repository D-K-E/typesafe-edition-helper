{-|
Module : DataUtils
License : see LICENSE
Description : Contains typeclasses that are common for models
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module DataUtils
    ( StringLikeCons
    , Model2StringText
    , Model2Map
    , ModelAttrMaker
    )
where
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Utils.MapUtils                 ( convertStringKey
                                                , convertStringVal
                                                , convertTxtMap2String
                                                )

class StringLikeCons model where
    fromString :: String -> model
    fromText :: Text -> model
    fromText aText = fromString (unpack aText)


class Model2StringText model where
    toString :: model -> String
    toText :: model -> Text


class ModelAttrMaker model where
    fromString :: Map String String -> model
    fromText :: Map Text Text -> model
    fromMixedStr :: Map String Text -> model
    fromMixedText :: Map Text String -> model

    fromMixedStr aMap = fromText (convertStringKey aMap)
    fromMixedText aMap = fromText (convertStringVal aMap)
    fromText aMap = fromString (convertTxtMap2String aMap)


class Model2Map model where
    toTxtMap :: model -> Map Text Text
    toStringMap :: model -> Map String String
