{-|
Module : Model
License : see LICENSE
Description : Container.hs is a controller for container model
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Container where

import           Model.Container                ( ContainerModel(..)
                                                , ContainerData(..)
                                                )
import           Model.ModelInfo               as MInfo
                                                ( ModelInfo(..) )
import           Model.ModelId                  ( ModelId )
import           Model.ModelType                ( ModelType )
import           Model.ModelAttr                ( ModelAttr )
import           Control.ModelInfo             as CMInfo
                                                ( changeModelInfoId
                                                , changeModelInfoType
                                                , changeModelInfoAttr
                                                )
import           Utils.ControlUtils             ( ReplaceField(..)
                                                , MatchModel(..)
                                                )


-- |'changeContainerInfo' change model info of given container model
changeContainerInfo :: ContainerModel -> ModelInfo -> ContainerModel
changeContainerInfo cmodel minfo =
    ContainerCons { modelInfo = minfo, modelData = modelData cmodel }

-- |'changeContainerId' change id of given container model
changeContainerId :: ContainerModel -> ModelId -> ContainerModel
changeContainerId cmodel mid = ContainerCons
    { modelInfo = CMInfo.changeModelInfoId (modelInfo cmodel) mid
    , modelData = modelData cmodel
    }

-- |'changeContainerType' change type of given container model
changeContainerType :: ContainerModel -> ModelType -> ContainerModel
changeContainerType cmodel mtype = ContainerCons
    { modelInfo = CMInfo.changeModelInfoType (modelInfo cmodel) mtype
    , modelData = modelData cmodel
    }

-- |'changeContainerAttr' change attribute of given container model
changeContainerAttr :: ContainerModel -> ModelAttr -> ContainerModel
changeContainerAttr cmodel mattr = ContainerCons
    { modelInfo = CMInfo.changeModelInfoAttr (modelInfo cmodel) mattr
    , modelData = modelData cmodel
    }
