{-|
Module : Model
License : see LICENSE
Description : ModelInfo primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Impure.ModelInfo
    ( ModelInfo(..)
    )
where

-- start definition
import           Primitive.Definition.ModelInfo ( ModelInfo
                                                    ( modelId
                                                    , modelType
                                                    , modelAttr
                                                    , InfoCons
                                                    )
                                                )
import           Primitive.Instance.Pure.ModelInfo
                                                ( ModelInfo )
-- end definition
-- start import functionality
import           Primitive.Instance.Impure.ModelId
                                                ( ModelId )
import           Primitive.Instance.Impure.ModelType
                                                ( ModelType )
import           Primitive.Instance.Impure.ModelAttr
                                                ( ModelAttr )

import           FunctionDef.Impure.Setter      ( StringLike2PrimitiveM(..) )
import           FunctionDef.Impure.Modifier    ( ReplaceInfoFieldM(..) )
import           FunctionDef.Impure.Transformer ( Model2StringTextM(..)
                                                , Model2MapM
                                                    ( toTextMapM
                                                    , toStringMapM
                                                    )
                                                )
import           Utils.MapUtils                 ( convertTxtMap2String )
import           Data.Map.Strict                ( fromList
                                                , Map
                                                , union
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Control.Monad.Fail            as Fail
                                                ( fail )
-- end import functionality

instance ReplaceInfoFieldM ModelInfo where
