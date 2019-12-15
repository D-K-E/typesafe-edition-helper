{-|
Module : Model
License : see LICENSE
Description : Unit primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Impure.Unit
    ( UnitModel
    )
where

-- start def
import           Primitive.Definition.Unit      ( UnitModel(UnitCons)
                                                , modelInfo
                                                , modelData
                                                )
import           Primitive.Instance.Pure.Unit   ( UnitModel )
-- end def
-- start functionality
import           Primitive.Definition.ModelData ( ModelData(UData, CData) )
import           Primitive.Instance.Impure.ModelData
                                                ( ModelData )
import           Primitive.Instance.Impure.ModelInfo
                                                ( ModelInfo
                                                    ( modelId
                                                    , modelType
                                                    , modelAttr
                                                    )
                                                )
import           Primitive.Instance.Impure.ModelAttr
                                                ( ModelAttr )
import           Primitive.Instance.Impure.ModelType
                                                ( ModelType )
import           Primitive.Instance.Impure.ModelId
                                                ( ModelId )

-- Function definition
import           FunctionDef.Instance.Impure.Modifier
                                                ( ReplaceInfoFieldM(..)
                                                , ReplaceFieldM(..)
                                                , Add2FieldM(..)
                                                )
import           FunctionDef.Instance.Impure.Matcher
                                                ( MatchModelM(..) )
import           FunctionDef.Instance.Impure.Transformer
                                                ( Model2TupleM
                                                , toTupleM
                                                , toStringMapM
                                                )
import           Data.Map.Strict                ( isSubmapOfBy
                                                , union
                                                )
import           Data.List                      ( isInfixOf )

instance Model2TupleM UnitModel where
    toTupleM model = return (modelInfo model, UData (modelData model))


instance ReplaceInfoFieldM UnitModel where
    replaceIdM umodel mid = return UnitCons
        { modelInfo = replaceId (modelInfo umodel) mid
        , modelData = modelData umodel
        }
    replaceTypeM umodel mtype = return UnitCons
        { modelInfo = replaceType (modelInfo umodel) mtype
        , modelData = modelData umodel
        }
    replaceAttrM umodel mattr = return UnitCons
        { modelInfo = replaceAttr (modelInfo umodel) mattr
        , modelData = modelData umodel
        }


instance ReplaceFieldM UnitModel where
    replaceDataM umodel (UData udata) =
        return UnitCons { modelInfo = modelInfo umodel, modelData = udata }
    replaceDataM umodel (CData cdata) =
        fail "only UnitData is accepted for replacement. ContainerData is given"
    replaceInfo umodel minfo =
        return UnitCons { modelInfo = minfo, modelData = modelData umodel }

instance MatchModelM UnitModel where
    hasSameData umodel (CData udata) =
        fail
            "only UnitData is accepted for\
        \ equality check. ContainerData is given"
    containsData umodel (UData udata) =
        toString (modelData umodel) `isInfixOf` toString udata
    containsData umodel (CData udata) =
        fail
            "only UnitData is accepted for\
        \ containement comparaison. ContainerData is given"

instance Add2FieldM UnitModel where

    append2Data model (CData mdata) =
        fail
            "Only UnitData is accepted for\
        \ appending data. ContainerData is given"

    prepend2Data model (CData mdata) =
        fail
            "Only UnitData is accepted for\
        \ appending data. ContainerData is given"
