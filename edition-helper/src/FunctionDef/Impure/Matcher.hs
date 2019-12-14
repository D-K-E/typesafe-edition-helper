{-|
Module : Matcher.hs
License : see LICENSE
Description : Contains typeclasses that are related to matching primitives
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Impure.Matcher
    ( MatchModelM(..)
    )
where

import           Primitive.Definition.ModelId   ( ModelId )
import           Primitive.Definition.ModelType ( ModelType )
import           Primitive.Definition.ModelAttr ( ModelAttr )
import           Primitive.Definition.ModelData ( ModelData )
import           FunctionDef.Pure.Matcher       ( MatchModel(..) )
import           Control.Monad                  ( Monad )


-- | 'MatchModel' class outlines methods to match model to given field
class (MatchModel model) => MatchModelM model where
    -- |'hasSameIdM' monadic version of 'hasSameId'
    hasSameIdM :: (Monad m) => model -> ModelId -> m Bool
    hasSameIdM = return hasSameId

    -- |'hasSameTypeM' monadic version of 'hasSameType'
    hasSameTypeM :: (Monad m) => model -> ModelType -> m Bool
    hasSameTypeM = return hasSameType

    -- |'hasSameAttr' whether Attr of given model is same with given model Attr
    hasSameAttrM :: (Monad m) => model -> ModelAttr -> m Bool
    hasSameAttrM = return hasSameAttr

    -- |'hasSameDataM' monadic version of 'hasSameData'
    hasSameDataM :: (Monad m) => model -> ModelData -> m Bool
    hasSameDataM = return hasSameData

    -- |'containsId' monadic version of 'containsId'
    containsIdM :: (Monad m) => model -> ModelId -> m Bool
    containsIdM = return containsId

    -- |'containsTypeM' monadic version of 'containsTypeM'
    containsTypeM :: (Monad m) => model -> ModelType -> m Bool
    containsTypeM = return containsType

    -- |'containsAttrM' monadic version of 'containsAttr'
    containsAttrM :: (Monad m) => model -> ModelAttr -> m Bool
    containsAttrM = return containsAttr

    -- |'containsDataM' monadic version of 'containsData'
    containsDataM :: (Monad m) => model -> ModelData -> m Bool
    containsDataM = return containsData
