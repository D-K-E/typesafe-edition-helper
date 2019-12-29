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
import           Primitive.Instance.ModelAttr
                                                ( ModelAttr(..) )
import           Primitive.Instance.ModelType
                                                ( ModelType(..) )
import           Primitive.Instance.ModelData
                                                ( ModelData(..) )
import           Primitive.Instance.ModelInfo
                                                ( ModelInfo(..) )
import           Primitive.Instance.Unit   ( UnitModel(..) )
import           Primitive.Instance.UnitData
                                                ( UnitData(..) )
-- end def

-- start fn

import           FunctionDef.Setter
import           FunctionDef.Transformer
import           FunctionDef.Matcher
import           FunctionDef.Modifier

-- end fn

import           Test.Hspec
