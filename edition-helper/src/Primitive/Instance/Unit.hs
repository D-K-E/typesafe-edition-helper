{-|
Module : Model
License : see LICENSE
Description : Unit primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Unit where


import           Primitive.Definition.Unit      ( UnitModel(UnitCons)
                                                , modelInfo
                                                , modelData
                                                )
import           Primitive.Definition.ModelData ( ModelData(UData, CData) )
import           Primitive.Instance.ModelInfo   ( ModelInfo
                                                    ( modelId
                                                    , modelType
                                                    , modelAttr
                                                    )
                                                )
import           Primitive.Instance.ModelAttr   ( ModelAttr )
import           Primitive.Instance.ModelType   ( ModelType )
import           Primitive.Instance.ModelId     ( ModelId )

-- Function definition
import           FunctionDef.Modifier           ( ReplaceInfoField(..)
                                                , ReplaceField(..)
                                                , Add2Field(..)
                                                )
import           FunctionDef.Matcher            ( MatchModel(..) )
import           View.Transformer               ( Model2Tuple
                                                , toTuple
                                                , toStringMap
                                                )
import           Data.Map.Strict                ( isSubmapOfBy
                                                , union
                                                )
import           Data.List                      ( isInfixOf )

instance Model2Tuple UnitModel where
    toTuple model = (modelInfo model, UData (modelData model))


instance ReplaceInfoField UnitModel where
    replaceId umodel mid = UnitCons
        { modelInfo = replaceId (modelInfo umodel) mid
        , modelData = modelData umodel
        }
    replaceType umodel mtype = UnitCons
        { modelInfo = replaceType (modelInfo umodel) mtype
        , modelData = modelData umodel
        }
    replaceAttr umodel mattr = UnitCons
        { modelInfo = replaceAttr (modelInfo umodel) mattr
        , modelData = modelData umodel
        }


instance ReplaceField UnitModel where
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
