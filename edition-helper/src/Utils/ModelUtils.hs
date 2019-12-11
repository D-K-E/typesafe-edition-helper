{-|
Module : DataUtils
License : see LICENSE
Description : Contains typeclasses that are common for models
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Utils.ModelUtils
    ( StringLikeCons(..)
    , ModelAttrMaker(..)
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

