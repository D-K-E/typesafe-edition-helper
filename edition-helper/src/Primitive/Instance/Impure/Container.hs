{-|
Module : Model
License : see LICENSE
Description : Container primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module Primitive.Instance.Impure.Container
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
import           Primitive.Instance.Pure.Container
                                                ( ContainerModel )
-- end of definition related imports

import           FunctionDef.Pure.Container     ( ContainerModel )
import           Primitive.Instance.Impure.ModelInfo
                                                ( ModelInfo )
import           FunctionDef.Impure.Modifier    ( ReplaceInfoFieldM
                                                , ReplaceFieldM(..)
                                                , Add2FieldM(..)
                                                )
import           FunctionDef.Impure.Matcher     ( MatchModelM(..) )
import           FunctionDef.Impure.Transformer ( Model2TupleM(..) )
import           Utils.StrUtils                 ( appendOrPrepend )

instance Model2TupleM ContainerModel where
    toTupleM model = return (modelInfo model, CData (modelData model))
    toTupleM model = fail "transforming container model to tuple has failed"

instance ReplaceInfoFieldM ContainerModel where
    replaceIdM cmodel mid = fail "replacing id of container model has failed"
    replaceTypeM cmodel mtype =
        fail "replacing type of container model has failed"
    replaceAttrM cmodel mattr =
        fail "replacing attributes of container model has failed"

instance ReplaceFieldM ContainerModel where
    replaceDataM cmodel (UData cdata) =
        fail "only ContainerData is accepted for replacement. UnitData is given"
    replaceInfoM cmodel minfo = fail "Replacing model info has failed"

instance MatchModelM ContainerModel where
    hasSameIdM cmodel mid =
        fail "equality comparison with given model id and model has failed"
    hasSameTypeM cmodel mtype =
        fail
            "equality comparison comparison with given\
            \ model type and model has failed"

    hasSameAttrM cmodel mattr =
        fail
            "equality comparison with given\
        \ model attribute and model has failed"
    containsAttrM cmodel mattr =
        fail
            "containement check failed for given\
        \ model attribute and model has failed"
    containsTypeM cmodel mtype =
        fail
            "containement check failed for given\
        \ model type and model has failed"

    containsIdM cmodel mid =
        fail
            "containement check failed for given\
        \ model id and model has failed"

    hasSameDataM cmodel (UData cdata) =
        fail
            "only ContainerData is accepted for\
        \ equality check. UnitData is given"

    containsDataM cmodel (UData cdata) =
        fail
            "only ContainerData is accepted for\
        \ containement comparaison. UnitData is given"


instance Add2FieldM ContainerModel where

    append2IdM model mid = fail "Appending model id to given model has failed"

    prepend2IdM model mid =
        fail "Prepending model id to given model has failed"
    append2TypeM model mtype =
        fail "Appending model type to given model has failed"

    prepend2TypeM model mtype =
        fail "Prepending model type to given model has failed"
    append2DataM model (UData mdata) =
        fail
            "Only ContainerData is accepted for\
        \ appending data. UnitData is given"

    prepend2DataM model (UData mdata) =
        fail
            "Only ContainerData is accepted for\
        \ appending data. UnitData is given"

    add2AttrM model mattr = fail "adding attributes to given model has failed"
