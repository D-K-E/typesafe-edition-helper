{-|
Module : Control.ModelType
License : see LICENSE
Description : monadic functions and their implementation dealing with model
type
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.ModelType where

import           Primitive.Instance.ModelType   ( ModelType )
import           Control.Monad                  ( Monad )
import           FunctionDef.Setter             ( StringLikeSetter(fromString) )

makeModelType :: (Monad m) => String -> m ModelType
makeModelType = fromString
