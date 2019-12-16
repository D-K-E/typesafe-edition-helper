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
import           FunctionDef.Impure.Modifier    ( ReplaceInfoFieldM(..)
                                                , ReplaceFieldM(..)
                                                , Add2FieldM(..)
                                                )
import           FunctionDef.Impure.Matcher     ( MatchModelM(..) )
import           FunctionDef.Impure.Setter      ( TupleLikeSetterM(..) )
import           FunctionDef.Impure.Transformer ( Model2TupleM
                                                , toTupleM
                                                , toStringMapM
                                                )
import           Data.Map.Strict                ( isSubmapOfBy
                                                , union
                                                )
import           Data.List                      ( isInfixOf )
import qualified Control.Monad.Fail            as Fail
                                                ( fail )

instance TupleLikeSetterM UnitModel where
    fromTupleM (minfo, CData mdata) =
        Fail.fail "Cannot set unit model from container data"

instance Model2TupleM UnitModel where
    toTupleM model = return (modelInfo model, UData (modelData model))


instance ReplaceInfoFieldM UnitModel where


instance ReplaceFieldM UnitModel where
    replaceDataM umodel (CData cdata) = Fail.fail
        "only UnitData is accepted for replacement. ContainerData is given"

instance MatchModelM UnitModel where
    hasSameDataM umodel (CData udata) =
        Fail.fail
            "only UnitData is accepted for\
        \ equality check. ContainerData is given"
    containsDataM umodel (CData udata) =
        Fail.fail
            "only UnitData is accepted for\
        \ containement comparaison. ContainerData is given"

instance Add2FieldM UnitModel where

    append2DataM model (CData mdata) =
        Fail.fail
            "Only UnitData is accepted for\
        \ appending data. ContainerData is given"

    prepend2DataM model (CData mdata) =
        Fail.fail
            "Only UnitData is accepted for\
        \ appending data. ContainerData is given"
