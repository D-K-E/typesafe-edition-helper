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
import           Utils.ModelUtils               ( StringLikeCons(..)
                                                , ModelAttrMaker(..)
                                                )
import           Utils.ViewUtils                ( Model2StringText(..)
                                                , Model2Map(..)
                                                )
import           Utils.StrUtils                 ( appendOrPrepend )
import           Utils.ControlUtils             ( ReplaceField(..)
                                                , MatchModel(..)
                                                , Add2Field(..)
                                                )
import           Data.List                      ( isInfixOf )
import           Data.Map.Strict                ( isSubmapOfBy
                                                , union
                                                )

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
    hasSameId umodel mid = modelId (modelInfo umodel) == mid
    hasSameType umodel mtype = modelType (modelInfo umodel) == mtype
    hasSameAttr umodel mattr = modelAttr (modelInfo umodel) == mattr
    containsAttr umodel mattr = isSubmapOfBy
        (==)
        (toStringMap (modelAttr (modelInfo umodel)))
        (toStringMap mattr)

    containsType umodel mtype =
        toString (modelType (modelInfo umodel)) `isInfixOf` toString mtype

    containsId umodel mid =
        toString (modelId (modelInfo umodel)) `isInfixOf` toString mid

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

instance Add2Field UnitModel where
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
                             (toString mtype)
                             True
            )
        )

    prepend2Type model mtype = replaceType
        model
        (fromString
            (appendOrPrepend (toString (modelType (modelInfo model)))
                             (toString mtype)
                             False
            )
        )

    add2Attr model mattr = replaceAttr
        model
        (fromStringMap
            (toStringMap (modelAttr (modelInfo model)) `union` toStringMap mattr
            )
        )

    append2Data model (UData mdata) = replaceData
        model
        (fromString
            (appendOrPrepend (toString (modelData model)) (toString mdata) True)
        )

    append2Data model (CData mdata) =
        error
            "Only UnitData is accepted for\
        \ appending data. ContainerData is given"

    prepend2Data model (UData mdata) = replaceData
        model
        (fromString
            (appendOrPrepend (toString (modelData model)) (toString mdata) False
            )
        )
