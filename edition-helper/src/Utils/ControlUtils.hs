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
    , Add2Field(..)
    )
where
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Map.Strict                ( Map
                                                , isSubmapOfBy
                                                , union
                                                )
import           Data.List                      ( isInfixOf )
import           Utils.MapUtils                 ( convertStringKey
                                                , convertStringVal
                                                , convertTxtMap2String
                                                )
import           Utils.StrUtils                 ( appendOrPrepend )
import           Utils.ViewUtils                ( Model2StringText(..)
                                                , Model2Map(..)
                                                )
import           Utils.ModelUtils               ( StringLikeCons(..) )
import           Model.ModelId                  ( ModelId )
import           Model.ModelType                ( ModelType )
import           Model.ModelInfo                ( ModelInfo(..) )
import           Model.ModelAttr                ( ModelAttr )
import           Model.ModelData                ( ModelData(..) )
import           Control.ModelInfo              ( changeModelInfoId
                                                , changeModelInfoType
                                                , changeModelInfoAttr
                                                )


-- | 'MatchModel' class outlines methods to match model to given field
class MatchModel model where
    -- |'hasSameId' whether id of given model is same with given model id
    hasSameId :: model -> ModelId -> Bool

    -- |'hasSameType' whether type of given model is same with given model type
    hasSameType :: model -> ModelType -> Bool

    -- |'hasSameAttr' whether Attr of given model is same with given model Attr
    hasSameAttr :: model -> ModelAttr -> Bool

    -- |'hasSameData' whether Data of given model is same with given model Data
    hasSameData :: model -> ModelData -> Bool

    -- |'containsId' whether model id of model contains given ModelId
    containsId :: model -> ModelId -> Bool

    -- |'containsType' whether model id of model contains given ModelType
    containsType :: model -> ModelType -> Bool

    -- |'containsAttr' whether model id of model contains given ModelAttr
    containsAttr :: model -> ModelAttr -> Bool

    -- |'containsData' whether Data of given model contains with given model Data
    containsData :: model -> ModelData -> Bool


-- | 'ReplaceField' class outlines methods to replace model to given field
class ReplaceField model where

    -- |'replaceId' replaces id of model with given ModelId
    replaceId :: model -> ModelId -> model
    -- |'replaceType' replaces Type of model with given ModelType
    replaceType :: model -> ModelType -> model

    -- |'replaceAttr' replaces Attr of model with given ModelAttr
    replaceAttr :: model -> ModelAttr -> model

    -- |'replaceInfo' replaces Info of model with given ModelInfo
    replaceInfo :: model -> ModelInfo -> model

    -- |'replaceData' replaces Data of model with given ModelData
    replaceData :: model -> ModelData -> model

-- | 'Add2Field' class outlines methods to add new value to given field
class (ReplaceField model) => Add2Field model where
    -- |'append2Id' append new model id at the end of model id
    append2Id :: model -> ModelId -> model

    -- |'prepend2Id' prepend new model id at the start of model id
    prepend2Id :: model -> ModelId -> model

    -- |'append2Type' append new model Type at the end of model Type
    append2Type :: model -> ModelType -> model

    -- |'prepend2Type' prepend new model Type at the start of model Type
    prepend2Type :: model -> ModelType -> model

    -- |'append2Attr' append new model Attr at the end of model Attr
    add2Attr :: model -> ModelAttr -> model

    -- |'append2Data' append new model Data at the end of model Data
    append2Data :: model -> ModelData -> model

    -- |'prepend2Data' prepend new model Data at the start of model Data
    prepend2Data :: model -> ModelData -> model
