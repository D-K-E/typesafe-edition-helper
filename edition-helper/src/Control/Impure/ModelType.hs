{-|
Module : Model
License : see LICENSE
Description : ModelType.hs monadic maker
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Impure.ModelType
    (-- makeModelTypeM
    )
where

import           Primitive.Instance.Impure.ModelType
                                                ( ModelType )
import           FunctionDef.Impure.Setter      ( StringLike2PrimitiveM
                                                    ( fromStringM
                                                    )
                                                )
import           Control.Monad                  ( Monad )

--makeModelTypeM :: (Monad m) => String -> m ModelType
--makeModelTypeM = fromStringM
