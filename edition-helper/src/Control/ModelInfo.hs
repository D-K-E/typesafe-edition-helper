{-|
Module : Model
License : see LICENSE
Description : ModelInfo.hs is a controller for unit model
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.ModelInfo
    ( changeModelInfoId
    , changeModelInfoType
    , changeModelInfoAttr
    )
where

import           Primitive.ModelInfo               as MInfo
                                                ( PrimitiveInfo(..) )
import           Primitive.ModelId                  ( ModelId )
import           Primitive.ModelType                ( ModelType )
import           Primitive.ModelAttr                ( ModelAttr )

-- |'changeModelInfoId' change id field of given model info
changeModelInfoId :: ModelInfo -> ModelId -> ModelInfo
changeModelInfoId minfo mid = InfoCons { modelId   = mid
                                       , modelType = MInfo.modelType minfo
                                       , modelAttr = MInfo.modelAttr minfo
                                       }

-- |'changeModelInfoType' change model type of given model info
changeModelInfoType :: ModelInfo -> ModelType -> ModelInfo
changeModelInfoType minfo mtype = InfoCons { modelId   = MInfo.modelId minfo
                                           , modelType = mtype
                                           , modelAttr = MInfo.modelAttr minfo
                                           }

-- |'changeModelInfoAttr' change model attribute of given model info
changeModelInfoAttr :: ModelInfo -> ModelAttr -> ModelInfo
changeModelInfoAttr minfo mattr = InfoCons { modelId   = MInfo.modelId minfo
                                           , modelType = MInfo.modelType minfo
                                           , modelAttr = mattr
                                           }
