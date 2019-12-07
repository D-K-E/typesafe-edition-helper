{-|
Module : Model
License : see LICENSE
Description : ModelInfo primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module ModelInfo
    ( ModelInfo(..)
    )
where

import           Model.ModelId                  ( ModelId )
import           Model.ModelType                ( ModelType )
import           Model.ModelAttr                ( ModelAttr )
import           Utils.DataUtils                ( StringLikeCons )
import           Data.Map                       ( fromList
                                                , Map
                                                , union
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )


-- | model info: contains meta data with regard to unit/container model
data ModelInfo = InfoCons {
      modelId :: ModelId
    , modelType :: ModelType
    , modelAttr :: ModelAttr
    } deriving (Eq, Show)

instance Model2Map ModelInfo where
    getModelIdTypeMap :: model -> Map Text Text
    getInfoMap :: model -> Map Text Text
    getModelIdTypeMap aModel =
        fromList [(pack "id", modelId aModel), (pack "type", modelType aModel)]

    getInfoMap aModel = union (getModelIdTypeMap aModel) (modelAttr aModel)
    toTxtMap aModel = getInfoMap aModel
    toStringMap aModel = convertTxtMap2String (getInfoMap aModel)
