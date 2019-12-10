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

instance ReplaceField UnitModel where
    replaceId cmodel mid = ContainerCons
        { modelInfo = CMInfo.changeModelInfoId (modelInfo cmodel) mid
        , modelData = modelData cmodel
        }
    replaceType cmodel mtype = ContainerCons
        { modelInfo = CMInfo.changeModelInfoType (modelInfo cmodel) mtype
        , modelData = modelData cmodel
        }
    replaceAttr cmodel mattr = ContainerCons
        { modelInfo = CMInfo.changeModelInfoAttr (modelInfo cmodel) mattr
        , modelData = modelData cmodel
        }
    replaceData cmodel (CData cdata) =
        ContainerCons { modelInfo = modelInfo cmodel, modelData = cdata }
    replaceData cmodel (UData cdata) = error
        "only ContainerData is accepted for replacement. UnitData is given"
    replaceInfo cmodel minfo =
        ContainerCons { modelInfo = minfo, modelData = modelData cmodel }

instance MatchModel ContainerModel where
    hasSameData cmodel (CData cdata) = modelData cmodel == cdata
    hasSameData cmodel (UData cdata) =
        error
            "only ContainerData is accepted for\
        \ equality check. UnitData is given"

    containsData cmodel (CData cdata) =
        toString (modelData cmodel) `isInfixOf` toString cdata
    containsData cmodel (UData cdata) =
        error
            "only ContainerData is accepted for\
        \ containement comparaison. UnitData is given"
