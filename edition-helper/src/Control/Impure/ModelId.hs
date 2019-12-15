{-|
Module : Model
License : see LICENSE
Description : ModelId.hs monadic maker
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Impure.ModelId
    ( makeModelIdM
    )
where

import           Primitive.Instance.Impure.ModelId
                                                ( ModelId )
import           FunctionDef.Impure.Setter      ( StringLikeSetterM(fromStringM)
                                                )
import           Control.Monad.Fail             ( MonadFail )

makeModelIdM :: (MonadFail m) => String -> m ModelId
makeModelIdM = fromStringM
