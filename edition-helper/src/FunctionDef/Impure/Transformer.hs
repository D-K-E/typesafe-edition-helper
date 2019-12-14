{-|
Module : Transformer
License : see LICENSE
Description : Regroups classes that transforms models into native data types
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module FunctionDef.Impure.Transformer
    ( Model2StringTextM(..)
    , Model2TupleM(..)
    , Model2MapM(..)
    )
where

-- start def
import           FunctionDef.Pure.Transformer   ( Model2StringText
                                                , Model2Tuple
                                                , Model2Map
                                                )
-- end def
-- start functionality
import           Control.Monad                  ( Monad )
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           Primitive.Definition.ModelData ( ModelData )
import           Primitive.Definition.ModelInfo ( ModelInfo )
-- end functionality

-- |'Model2StringTextM' monadic version of 'Model2StringText'
class (Model2StringText model) => Model2StringTextM model where
    toStringM :: (Monad m) => model -> m String
    toTextM :: (Monad m) => model -> m Text

-- |'Model2TupleM' monadic version of 'Model2Tuple'
class (Model2Tuple model) => Model2TupleM model where
    toTupleM :: (Monad m) => model -> m (ModelInfo, ModelData)

-- |'Model2MapM' monadic version of 'Model2Map'
class (Model2Map model) => (Model2MapM model) where
    toTextMapM :: (Monad m) model -> m Map Text Text
    toStringMapM :: (Monad m) model -> m Map String String
