{-|
Module : Model
License : see LICENSE
Description : ModelInfo primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Model.ModelInfo
    ( ModelInfo(..)
    )
where

import           Model.ModelId                  ( ModelId )
import           Model.ModelType                ( ModelType )
import           Model.ModelAttr                ( ModelAttr )
import           Utils.ModelUtils               ( StringLikeCons(..) )
import           Utils.ViewUtils                ( Model2StringText(..)
                                                , Model2Map(..)
                                                )
import           Utils.MapUtils                 ( convertTxtMap2String )
import           Data.Map.Strict                ( fromList
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


-- |'getModelIdTypeMap' transform model id type field
-- to map with key value both as Text
getModelIdTypeMap :: ModelInfo -> Map Text Text
getModelIdTypeMap aModel = fromList
    [ (pack "id"  , toText (modelId aModel))
    , (pack "type", toText (modelType aModel))
    ]

-- |'getInfoMap' transform model info to map with key value both as Text
getInfoMap :: ModelInfo -> Map Text Text
getInfoMap aModel =
    getModelIdTypeMap aModel `union` toTxtMap (modelAttr aModel)


-- | convert model info to map
instance Model2Map ModelInfo where
    toTxtMap = getInfoMap
    toStringMap aModel = convertTxtMap2String (getInfoMap aModel)
