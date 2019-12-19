{-|
Module : Model
License : see LICENSE
Description : ModelInfo primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Pure.ModelInfo
    ( ModelInfo
    )
where

import           Primitive.Definition.ModelInfo ( ModelInfo
                                                    ( modelId
                                                    , modelType
                                                    , modelAttr
                                                    , InfoCons
                                                    )
                                                )
import           Primitive.Instance.Pure.ModelId
                                                ( ModelId(..) )
import           Primitive.Instance.Pure.ModelType
                                                ( ModelType(..) )
import           Primitive.Instance.Pure.ModelAttr
                                                ( ModelAttr(..) )
import           FunctionDef.Pure.Setter        ( InfoTuple2Primitive(..) )
import           FunctionDef.Pure.Modifier      ( ReplaceInfoField(..) )
import           FunctionDef.Pure.Transformer   ( Model2IdTuple(toIdTuple)
                                                , ModelInfo2Tuple(..)
                                                )
import           Utils.MapUtils                 ( convertTxtMap2String )
import           Data.Map.Strict                ( fromList
                                                , Map
                                                , union
                                                )
import qualified Data.Map.Strict               as Mp
                                                ( lookup
                                                , delete
                                                , (!)
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )

-- start setter
instance InfoTuple2Primitive ModelInfo where
    fromInfoTuple (mid, mtype, mattr) =
        InfoCons { modelId = mid, modelType = mtype, modelAttr = mattr }

-- end setter

-- start transform

instance Model2IdTuple ModelInfo where
    toIdTuple minfo = ("info", minfo)

instance ModelInfo2Tuple ModelInfo where
    toInfoTuple model = (modelId model, modelType model, modelAttr model)

-- end transform
instance ReplaceInfoField ModelInfo where
    replaceId minfo mid = InfoCons { modelId   = mid
                                   , modelType = modelType minfo
                                   , modelAttr = modelAttr minfo
                                   }
    replaceType minfo mtype = InfoCons { modelId   = modelId minfo
                                       , modelType = mtype
                                       , modelAttr = modelAttr minfo
                                       }
    replaceAttr minfo mattr = InfoCons { modelId   = modelId minfo
                                       , modelType = modelType minfo
                                       , modelAttr = mattr
                                       }
