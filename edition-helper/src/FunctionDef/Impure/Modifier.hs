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

import           Control.Monad             ( Monad )


-- | 'ReplaceInfoFieldM' class generalizes 'ReplaceInfoField' to monad
class (ReplaceInfoField model) => ReplaceInfoFieldM model where

    -- |'replaceIdM' monadic version of 'replaceId'
    replaceIdM :: (Monad m) => model -> ModelId -> m model
    replaceIdM model mid = return (replaceId model mid)
    -- |'replaceTypeM' monadic version of 'replaceType'
    replaceTypeM :: (Monad m) => model -> ModelType -> m model
    replaceTypeM model mtype = return (replaceType model mtype)
    -- |'replaceAttrM' monadic version of 'replaceAttr'
    replaceAttrM :: (Monad m) => model -> ModelAttr -> m model
    replaceAttrM model mattr = return (replaceAttr model mattr)


-- | 'ReplaceFieldM' generalizes ReplaceField class to monads
class (ReplaceField model) => ReplaceFieldM model where

    -- |'replaceInfoM' monadic version of 'replaceInfo'
    replaceInfoM :: (Monad m) => model -> ModelInfo -> m model
    replaceInfoM model minfo = return (replaceInfo model minfo)
    -- |'replaceDataM' monadic version of 'replaceData'
    replaceDataM :: (Monad m) => model -> ModelData -> m model
    replaceDataM model mdata = return (replaceData model mdata)

-- | 'Add2FieldM' class outlines methods to add new value to given field
class (Add2Field model) => Add2FieldM model where
    -- |'append2IdM' monadic version of 'append2Id'
    append2IdM :: (Monad m) => model -> ModelId -> m model
    append2IdM model mid = return (append2Id model mid)

    -- |'prepend2IdM' monadic version of 'prepend2Id'
    prepend2IdM :: (Monad m) => model -> ModelId -> m model
    prepend2IdM model mid = return (prepend2Id model mid)

    -- |'append2TypeM' monadic version of 'append2Type'
    append2TypeM :: (Monad m) => model -> ModelType -> m model
    append2TypeM model mtype = return (append2Type model mtype)

    -- |'prepend2TypeM' monadic version of 'prepend2Type'
    prepend2TypeM :: (Monad m) => model -> ModelType -> m model
    prepend2TypeM model mtype = return (prepend2Type model mtype)

    -- |'add2AttrM' monadic version of 'add2Attr'
    add2AttrM :: (Monad m) => model -> ModelAttr -> m model
    add2AttrM model mattr = return (add2Attr model mattr)

    -- |'append2DataM' monadic version of 'append2Data'
    append2DataM :: (Monad m) => model -> ModelData -> m model
    append2DataM model mdata = return (append2Data model mdata)

    -- |'prepend2DataM' monadic version of 'prepend2Data'
    prepend2DataM :: (Monad m) => model -> ModelData -> m model
    prepend2DataM model mdata = return (prepend2Data model mdata)
