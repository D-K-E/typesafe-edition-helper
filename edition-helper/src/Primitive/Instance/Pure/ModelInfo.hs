{-|
Module : Model
License : see LICENSE
Description : ModelInfo primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Pure.ModelInfo
    ( ModelInfo
    )
where

import           Primitive.Definition.ModelInfo ( ModelInfo
                                                    ( modelId
                                                    , modelType
                                                    , modelAttr
                                                    , InfoCons
                                                    )
                                                )
import           Primitive.Instance.Pure.ModelId
                                                ( ModelId(..) )
import           Primitive.Instance.Pure.ModelType
                                                ( ModelType(..) )
import           Primitive.Instance.Pure.ModelAttr
                                                ( ModelAttr(..) )
import           FunctionDef.Pure.Setter        ( StringLike2Primitive(..)
                                                , Map2Primitive(..)
                                                )
import           FunctionDef.Pure.Modifier      ( ReplaceInfoField(..) )
import           FunctionDef.Pure.Transformer   ( Model2StringText(..)
                                                , Model2Map
                                                    ( toTextMap
                                                    , toStringMap
                                                    )
                                                , Model2IdTuple(toIdTuple)
                                                )
import           Utils.MapUtils                 ( convertTxtMap2String )
import           Data.Map.Strict                ( fromList
                                                , Map
                                                , union
                                                )
import qualified Data.Map.Strict               as Mp
                                                ( lookup
                                                , delete
                                                , (!)
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

-- start setter

instance Map2Primitive ModelInfo where
    fromStringMap minfo = InfoCons { modelId   = mid
                                   , modelType = mtype
                                   , modelAttr = mattr
                                   }
      where
        mid   = fromString (minfo Mp.! "id")
        mtype = fromString (minfo Mp.! "type")
        mattr = fromStringMap (Mp.delete "type" (Mp.delete "id" minfo))

-- end setter

instance Model2IdTuple ModelInfo where
    toIdTuple minfo = ("info", minfo)

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
