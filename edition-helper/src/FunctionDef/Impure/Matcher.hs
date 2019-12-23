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
import           Control.Monad             ( Monad )


-- | 'MatchModel' class outlines methods to match model to given field
class (MatchModel model) => MatchModelM model where
    -- |'hasSameIdM' monadic version of 'hasSameId'
    hasSameIdM :: (Monad m) => model -> ModelId -> m Bool
    hasSameIdM model mid = return (hasSameId model mid)

    -- |'hasSameTypeM' monadic version of 'hasSameType'
    hasSameTypeM :: (Monad m) => model -> ModelType -> m Bool
    hasSameTypeM model mtype = return (hasSameType model mtype)

    -- |'hasSameAttr' whether Attr of given model is same with given model Attr
    hasSameAttrM :: (Monad m) => model -> ModelAttr -> m Bool
    hasSameAttrM model mattr = return (hasSameAttr model mattr)

    -- |'hasSameDataM' monadic version of 'hasSameData'
    hasSameDataM :: (Monad m) => model -> ModelData -> m Bool
    hasSameDataM model mdata = return (hasSameData model mdata)

    -- |'containsId' monadic version of 'containsId'
    containsIdM :: (Monad m) => model -> ModelId -> m Bool
    containsIdM model mid = return (containsId model mid)

    -- |'containsTypeM' monadic version of 'containsTypeM'
    containsTypeM :: (Monad m) => model -> ModelType -> m Bool
    containsTypeM model mtype = return (containsType model mtype)

    -- |'containsAttrM' monadic version of 'containsAttr'
    containsAttrM :: (Monad m) => model -> ModelAttr -> m Bool
    containsAttrM model mattr = return (containsAttr model mattr)

    -- |'containsDataM' monadic version of 'containsData'
    containsDataM :: (Monad m) => model -> ModelData -> m Bool
    containsDataM model mdata = return (containsData model mdata)
