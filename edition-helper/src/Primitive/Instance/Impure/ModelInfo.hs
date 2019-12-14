{-|
Module : Model
License : see LICENSE
Description : ModelInfo primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Impure.ModelInfo
    ( ModelInfoM(..)
    )
where

-- start definition
import           Primitive.Definition.ModelInfo ( ModelInfo
                                                    ( modelId
                                                    , modelType
                                                    , modelAttr
                                                    , InfoCons
                                                    )
                                                )
import           Primitive.Instance.Pure.ModelInfo
                                                ( ModelInfo )
-- end definition
-- start import functionality
import           Primitive.Instance.Impure.ModelId
                                                ( ModelId )
import           Primitive.Instance.Impure.ModelType
                                                ( ModelType )
import           Primitive.Instance.Impure.ModelAttr
                                                ( ModelAttr )

import           FunctionDef.Impure.Setter      ( StringLikeSetterM(..) )
import           FunctionDef.Impure.Modifier    ( ReplaceInfoFieldM(..) )
import           View.Transformer               ( Model2StringText(..)
                                                , Model2Map
                                                , toTextMap
                                                , toStringMap
                                                )
import           Utils.MapUtils                 ( convertTxtMap2String )
import           Data.Map.Strict                ( fromList
                                                , Map
                                                , union
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
-- end import functionality

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
    getModelIdTypeMap aModel `union` toTextMap (modelAttr aModel)


instance ReplaceInfoFieldM ModelInfo where
    replaceIdM minfo mid = fail "replacing id of model info has failed"
    replaceTypeM minfo mtype = fail "replacing type of model info has failed"
    replaceAttrM minfo mattr =
        fail "replacing attribute of model info has failed"
