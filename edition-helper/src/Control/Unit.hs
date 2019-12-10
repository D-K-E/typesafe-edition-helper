{-|
Module : Model
License : see LICENSE
Description : Unit.hs is a controller for unit model
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Unit
    ()
where
import           Model.Unit                     ( UnitModel(..) )
import           Model.UnitData                 ( UnitData(..) )
import           Model.ModelInfo               as MInfo
                                                ( ModelInfo(..) )
import           Model.ModelId                  ( ModelId )
import           Model.ModelType                ( ModelType )
import           Model.ModelAttr                ( ModelAttr )
import           Model.ModelData                ( ModelData(..) )
import           Control.ModelInfo             as CMInfo
                                                ( changeModelInfoId
                                                , changeModelInfoType
                                                , changeModelInfoAttr
                                                )
import           Utils.ViewUtils                ( Model2StringText(..)
                                                , Model2Map(..)
                                                )
import           Utils.ControlUtils             ( ReplaceField(..)
                                                , MatchModel(..)
                                                )
import           Data.List                      ( isInfixOf )
import           Data.Map                       ( isSubmapOfBy )

instance ReplaceField UnitModel where
    replaceId umodel mid = UnitCons
        { modelInfo = CMInfo.changeModelInfoId (modelInfo umodel) mid
        , modelData = modelData umodel
        }
    replaceType umodel mtype = UnitCons
        { modelInfo = CMInfo.changeModelInfoType (modelInfo umodel) mtype
        , modelData = modelData umodel
        }
    replaceAttr umodel mattr = UnitCons
        { modelInfo = CMInfo.changeModelInfoAttr (modelInfo umodel) mattr
        , modelData = modelData umodel
        }
    replaceData umodel (UData udata) =
        UnitCons { modelInfo = modelInfo umodel, modelData = udata }
    replaceData umodel (CData cdata) = error
        "only UnitData is accepted for replacement. ContainerData is given"
    replaceInfo umodel minfo =
        UnitCons { modelInfo = minfo, modelData = modelData umodel }

instance MatchModel UnitModel where
    hasSameData umodel (UData udata) = modelData umodel == udata
    hasSameData umodel (CData udata) =
        error
            "only UnitData is accepted for\
        \ equality check. ContainerData is given"
    containsData umodel (UData udata) =
        toString (modelData umodel) `isInfixOf` toString udata
    containsData umodel (CData udata) =
        error
            "only UnitData is accepted for\
        \ containement comparaison. ContainerData is given"
