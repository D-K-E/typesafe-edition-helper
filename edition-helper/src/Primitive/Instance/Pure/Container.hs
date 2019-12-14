{-|
Module : Model
License : see LICENSE
Description : Container primitive pure instance
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module Primitive.Instance.Container
    ( ContainerModel
    , ContainerData
    )
where

-- definition related imports
import           Primitive.Definition.Container ( modelInfo
                                                , modelData
                                                , ContainerModel(ContainerCons)
                                                )
import           Primitive.Definition.ModelData ( ModelData(CData) )
-- end of definition related imports

import           Primitive.Instance.ModelInfo   ( ModelInfo )
import           FunctionDef.Pure.Modifier      ( ReplaceInfoField
                                                , ReplaceField(..)
                                                , Add2Field(..)
                                                )
import           FunctionDef.Matcher            ( MatchModel(..) )
import           View.Transformer               ( Model2Tuple(..) )
import           Utils.StrUtils                 ( appendOrPrepend )

instance Model2Tuple ContainerModel where
    toTuple model = (modelInfo model, CData (modelData model))


instance ReplaceInfoField ContainerModel where
    replaceId cmodel mid = ContainerCons
        { modelInfo = replaceId (modelInfo cmodel) mid
        , modelData = modelData cmodel
        }
    replaceType cmodel mtype = ContainerCons
        { modelInfo = replaceType (modelInfo cmodel) mtype
        , modelData = modelData cmodel
        }
    replaceAttr cmodel mattr = ContainerCons
        { modelInfo = replaceAttr (modelInfo cmodel) mattr
        , modelData = modelData cmodel
        }

instance ReplaceField ContainerModel where
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
