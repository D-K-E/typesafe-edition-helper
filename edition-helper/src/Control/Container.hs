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
import           Model.ModelData                ( ModelData(..) )
import           Control.ModelInfo             as CMInfo
                                                ( changeModelInfoId
                                                , changeModelInfoType
                                                , changeModelInfoAttr
                                                )
import           Utils.StrUtils                 ( appendOrPrepend )
import           Utils.ControlUtils             ( ReplaceField(..)
                                                , MatchModel(..)
                                                , Add2Field(..)
                                                )

instance ReplaceField ContainerModel where
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
    hasSameId cmodel mid = cmodelId (modelInfo cmodel) == mid
    hasSameType cmodel mtype = cmodelType (modelInfo cmodel) == mtype
    hasSameAttr cmodel mattr = cmodelAttr (modelInfo cmodel) == mattr
    containsAttr cmodel mattr = isSubmapOfBy
        (==)
        (toStringMap (modelAttr (modelInfo cmodel)))
        (toStringMap mattr)

    containsType cmodel mtype =
        toString (modelType (modelInfo cmodel)) `isInfixOf` toString mtype

    containsId cmodel mid =
        toString (modelId (modelInfo cmodel)) `isInfixOf` toString mid

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

instance Add2Field ContainerModel where
    append2Id model mid = replaceId
        model
        (fromString
            (appendOrPrepend (toString (modelId (modelInfo model)))
                             (toString mid)
                             True
            )
        )

    prepend2Id model mid = replaceId
        model
        (fromString
            (appendOrPrepend (toString (modelId (modelInfo model)))
                             (toString mid)
                             False
            )
        )

    append2Type model mtype = replaceType
        model
        (fromString
            (appendOrPrepend (toString (modelType (modelInfo model)))
                             (toString mid)
                             True
            )
        )

    prepend2Type model mtype = replaceType
        model
        (fromString
            (appendOrPrepend (toString (modelType (modelInfo model)))
                             (toString mid)
                             False
            )
        )

    append2Data model (CData mdata) =
        replaceData model (modelData model ++ mdata)
    append2Data model (UData mdata) =
        error
            "Only ContainerData is accepted for\
        \ appending data. UnitData is given"

    prepend2Data model (CData mdata) =
        replaceData model (mdata ++ modelData model)

    add2Attr model mattr = replaceAttr
        model
        fromStringMap
        (toStringMap (modelAttr (modelInfo model)) `union` toStringMap mattr)
