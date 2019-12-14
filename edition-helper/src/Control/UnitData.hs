{-|
Module : Control.UnitData
License : see LICENSE
Description : monadic functions and their implementation dealing with unit data
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.UnitData where

import           Primitive.Instance.UnitData    ( UnitData )
import           Control.Monad                  ( Monad )
import           FunctionDef.Setter             ( StringLikeSetter(fromString) )

makeUData :: (Monad m) => String -> m UnitData
makeUData = fromString
