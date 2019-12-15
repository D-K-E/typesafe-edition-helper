{-|
Module : Model
License : see LICENSE
Description : ModelType.hs monadic maker
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Impure.ModelType
    ( makeModelTypeM
    )
where

import           Primitive.Instance.Impure.ModelType
                                                ( ModelType )
import           FunctionDef.Impure.Setter      ( StringLikeSetterM(fromStringM)
                                                )
import           Control.Monad.Fail             ( MonadFail )

makeModelTypeM :: (MonadFail m) => String -> m ModelType
makeModelTypeM = fromStringM
