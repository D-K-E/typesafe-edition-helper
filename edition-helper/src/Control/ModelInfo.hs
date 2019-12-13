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

import           Primitive.Definition.ModelInfo
                                               as MInfo
                                                ( ModelInfo(..) )
import           Primitive.Definition.ModelId   ( ModelId )
import           Primitive.Definition.ModelType ( ModelType )
import           Primitive.Definition.ModelAttr ( ModelAttr )

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
