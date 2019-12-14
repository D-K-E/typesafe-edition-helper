{-|
Module : Control.ModelAttr
License : see LICENSE
Description : monadic functions and their implementation dealing with model
attribute
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.ModelAttr where
import           Primitive.Instance.ModelAttr   ( ModelAttr )
import           Control.Monad                  ( Monad )
import           FunctionDef.Setter             ( ModelAttrSetter(fromStringMap)
                                                )

makeModelAttr :: (Monad m) => Map String String -> m ModelAttr
makeModelAttr = fromStringMap
