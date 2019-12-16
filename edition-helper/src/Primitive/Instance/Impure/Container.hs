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
    )
where

-- definition related imports
import           Primitive.Definition.Container ( modelInfo
                                                , modelData
                                                , ContainerModel(ContainerCons)
                                                )
import           Primitive.Definition.ModelData ( ModelData(CData, UData) )
import           Primitive.Instance.Pure.Container
                                                ( ContainerModel )
-- end of definition related imports

import           Primitive.Instance.Impure.ModelInfo
                                                ( ModelInfo )
import           FunctionDef.Impure.Modifier    ( ReplaceInfoFieldM
                                                , ReplaceFieldM(..)
                                                , Add2FieldM(..)
                                                )
import           FunctionDef.Impure.Matcher     ( MatchModelM(..) )
import           FunctionDef.Impure.Transformer ( Model2TupleM(..) )
import           Utils.StrUtils                 ( appendOrPrepend )
import qualified Control.Monad.Fail            as Fail
                                                ( fail )

instance Model2TupleM ContainerModel where

instance ReplaceInfoFieldM ContainerModel where

instance ReplaceFieldM ContainerModel where
    replaceDataM cmodel (UData cdata) = Fail.fail
        "only ContainerData is accepted for replacement. UnitData is given"

instance MatchModelM ContainerModel where

    hasSameDataM cmodel (UData cdata) =
        Fail.fail
            "only ContainerData is accepted for\
        \ equality check. UnitData is given"

    containsDataM cmodel (UData cdata) =
        Fail.fail
            "only ContainerData is accepted for\
        \ containement comparaison. UnitData is given"


instance Add2FieldM ContainerModel where

    append2DataM model (UData mdata) =
        Fail.fail
            "Only ContainerData is accepted for\
        \ appending data. UnitData is given"

    prepend2DataM model (UData mdata) =
        Fail.fail
            "Only ContainerData is accepted for\
        \ appending data. UnitData is given"
