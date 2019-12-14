{-|
Module : Control.ModelId
License : see LICENSE
Description : monadic functions and their implementation dealing with model Id
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.ModelId where


import           Primitive.Instance.ModelId     ( ModelId )
import           Control.Monad                  ( Monad )
import           FunctionDef.Setter             ( StringLikeSetter(fromString) )

makeModelId :: (Monad m) => String -> m ModelId
makeModelId = fromString
