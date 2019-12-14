{-|
Module : Modifier.hs
License : see LICENSE
Description : Contains typeclasses that are related to modifying primitives
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Impure.Modifier
    ( ReplaceInfoFieldM(..)
    , ReplaceFieldM(..)
    , Add2FieldM(..)
    )
where

import           FunctionDef.Pure.Modifier      ( ReplaceField(..)
                                                , ReplaceInfoField(..)
                                                , Add2Field(..)
                                                )
import           Primitive.Definition.ModelId   ( ModelId )
import           Primitive.Definition.ModelType ( ModelType )
import           Primitive.Definition.ModelAttr ( ModelAttr )
import           Primitive.Definition.ModelData ( ModelData )
import           Primitive.Definition.ModelInfo ( ModelInfo )

import           Control.Monad.Fail             ( MonadFail )


-- | 'ReplaceInfoFieldM' class generalizes 'ReplaceInfoField' to monad
class (ReplaceInfoField model) => ReplaceInfoFieldM model where

    -- |'replaceIdM' monadic version of 'replaceId'
    replaceIdM :: (MonadFail m) => model -> ModelId -> m model
    replaceIdM = return replaceId
    -- |'replaceTypeM' monadic version of 'replaceType'
    replaceTypeM :: (MonadFail m) => model -> ModelType -> m model
    replaceTypeM = return replaceType
    -- |'replaceAttrM' monadic version of 'replaceAttr'
    replaceAttr :: model -> ModelAttr -> model
    replaceAttrM = return replaceAttr


-- | 'ReplaceField' generalizes ReplaceField class to monads
class (ReplaceField model) => ReplaceFieldM model where

    -- |'replaceInfoM' monadic version of 'replaceInfo'
    replaceInfoM :: (MonadFail m) => model -> ModelInfo -> m model
    replaceInfoM = return replaceInfo
    -- |'replaceDataM' monadic version of 'replaceData'
    replaceDataM :: (MonadFail m) => model -> ModelData -> m model
    replaceDataM = return replaceData

-- | 'Add2Field' class outlines methods to add new value to given field
class (Add2Field model) => Add2FieldM model where
    -- |'append2Id' append new model id at the end of model id
    append2IdM :: (MonadFail m) => model -> ModelId -> m model

    -- |'prepend2Id' prepend new model id at the start of model id
    prepend2IdM :: (MonadFail m) => model -> ModelId -> m model

    -- |'append2Type' append new model Type at the end of model Type
    append2TypeM :: (MonadFail m) => model -> ModelType -> m model

    -- |'prepend2Type' prepend new model Type at the start of model Type
    prepend2TypeM :: (MonadFail m) => model -> ModelType -> m model

    -- |'append2Attr' append new model Attr at the end of model Attr
    add2AttrM :: (MonadFail m) => model -> ModelAttr -> m model

    -- |'append2Data' append new model Data at the end of model Data
    append2DataM :: (MonadFail m) => model -> ModelData -> m model

    -- |'prepend2Data' prepend new model Data at the start of model Data
    prepend2DataM :: (MonadFail m) => model -> ModelData -> m model
