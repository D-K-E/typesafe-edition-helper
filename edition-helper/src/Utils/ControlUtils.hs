{-|
Module : ControlUtils
License : see LICENSE
Description : Contains typeclasses that are common for control related aspects
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Utils.ControlUtils
    ( MatchModel(..)
    , ReplaceField(..)
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
import           Utils.ViewUtils                (Model2StringText(..) )
import           Model.ModelId                  ( ModelId )
import           Model.ModelType                ( ModelType )
import           Model.ModelInfo                ( ModelInfo(..) )
import           Model.ModelAttr                ( ModelAttr )
import           Model.ModelData                ( ModelData(..) )

class MatchModel model where
    hasSameId :: model -> ModelId -> Bool
    hasSameType :: model -> ModelType -> Bool
    hasSameAttr :: model -> ModelAttr -> Bool
    hasSameData :: model -> ModelData -> Bool
    containsId :: model -> ModelId -> Bool
    containsType :: model -> ModelType -> Bool
    containsAttr :: model -> ModelAttr -> Bool
    containsData :: model -> ModelData -> Bool

class ReplaceField model where
    replaceId :: model -> ModelId -> model
    replaceType :: model -> ModelType -> model
    replaceAttr :: model -> ModelAttr -> model
    replaceInfo :: model -> ModelInfo -> model
    replaceData :: model -> ModelData -> model
