{-|
Module : Model
License : see LICENSE
Description : Test suit for primitive pure instance spec
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module PrimitiveTest.Pure.PrimitivePureSpec where

-- start def
import           Primitive.Definition.Container ( ContainerModel(..)
                                                , ContainerData(..)
                                                )
import           Primitive.Definition.ModelAttr ( ModelAttr(..) )
import           Primitive.Definition.ModelType ( ModelType(..) )
import           Primitive.Definition.ModelData ( ModelData(..) )
import           Primitive.Definition.ModelInfo ( ModelInfo(..) )
import           Primitive.Definition.Unit      ( UnitModel(..) )
import           Primitive.Definition.UnitData  ( UnitData(..) )

import           Primitive.Definition.Container ( ContainerModel(..)
                                                , ContainerData(..)
                                                )
import           Primitive.Instance.Pure.ModelAttr
                                                ( ModelAttr(..) )
import           Primitive.Instance.Pure.ModelType
                                                ( ModelType(..) )
import           Primitive.Instance.Pure.ModelData
                                                ( ModelData(..) )
import           Primitive.Instance.Pure.ModelInfo
                                                ( ModelInfo(..) )
import           Primitive.Instance.Pure.Unit   ( UnitModel(..) )
import           Primitive.Instance.Pure.UnitData
                                                ( UnitData(..) )
-- end def

-- start fn

import           FunctionDef.Pure.Setter
import           FunctionDef.Pure.Transformer
import           FunctionDef.Pure.Matcher
import           FunctionDef.Pure.Modifier

-- end fn

import           Test.Hspec
