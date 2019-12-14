{-|
Module : Model
License : see LICENSE
Description : ModelInfo primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.ModelInfo
    ( ModelInfo(..)
    )
where

import           Primitive.Definition.ModelInfo ( ModelInfo
                                                    ( modelId
                                                    , modelType
                                                    , modelAttr
                                                    , InfoCons
                                                    )
                                                )
import           Primitive.Instance.ModelId     ( ModelId )
import           Primitive.Instance.ModelType   ( ModelType )
import           Primitive.Instance.ModelAttr   ( ModelAttr )
import           FunctionDef.Setter             ( StringLikeSetter(..) )
import           FunctionDef.Modifier           ( ReplaceInfoField(..) )
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


-- | convert model info to map
instance Model2Map ModelInfo where
    toTextMap = getInfoMap
    toStringMap aModel = convertTxtMap2String (getInfoMap aModel)

instance ReplaceInfoField ModelInfo where
    replaceId minfo mid = InfoCons { modelId   = mid
                                   , modelType = modelType minfo
                                   , modelAttr = modelAttr minfo
                                   }
    replaceType minfo mtype = InfoCons { modelId   = modelId minfo
                                       , modelType = mtype
                                       , modelAttr = modelAttr minfo
                                       }
    replaceAttr minfo mattr = InfoCons { modelId   = modelId minfo
                                       , modelType = modelType minfo
                                       , modelAttr = mattr
                                       }
