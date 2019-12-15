{-|
Module : Model
License : see LICENSE
Description : Unit primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Pure.Unit where

-- start def
import           Primitive.Definition.Unit      ( UnitModel
                                                    ( UnitCons
                                                    , modelInfo
                                                    , modelData
                                                    )
                                                )
import           Primitive.Definition.ModelData ( ModelData(UData, CData) )
import           Primitive.Definition.ModelInfo ( ModelInfo
                                                    ( modelId
                                                    , modelType
                                                    , modelAttr
                                                    )
                                                )
import           Primitive.Instance.Pure.ModelInfo
                                                ( ModelInfo )
import           Primitive.Instance.Pure.ModelAttr
                                                ( ModelAttr )
import           Primitive.Instance.Pure.ModelType
                                                ( ModelType )
import           Primitive.Instance.Pure.ModelId
                                                ( ModelId )
import           Primitive.Instance.Pure.UnitData
                                                ( UnitData )
import           Primitive.Instance.Pure.ModelData
                                                ( ModelData )

-- end def
-- Function definition
import           FunctionDef.Pure.Setter        ( StringLikeSetter
                                                    ( fromString
                                                    , fromText
                                                    )
                                                , ModelAttrSetter(fromStringMap)
                                                )

import           FunctionDef.Pure.Modifier      ( ReplaceInfoField(..)
                                                , ReplaceField(..)
                                                , Add2Field(..)
                                                )
import           FunctionDef.Pure.Matcher       ( MatchModel(..) )
import           FunctionDef.Pure.Transformer   ( Model2Tuple(toTuple)
                                                , Model2Map
                                                    ( toStringMap
                                                    , toTextMap
                                                    )
                                                , Model2StringText
                                                    ( toString
                                                    , toText
                                                    )
                                                )
import           Utils.StrUtils                 ( appendOrPrepend )
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
    containsData umodel (UData udata) =
        toString (modelData umodel) `isInfixOf` toString udata

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

    prepend2Data model (UData mdata) = replaceData
        model
        (fromString
            (appendOrPrepend (toString (modelData model)) (toString mdata) False
            )
        )
