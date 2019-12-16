{-|
Module : Model
License : see LICENSE
Description : ContainerData primitive pure instance
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Instance.Pure.ContainerData
    ( ContainerData
    )
where

-- start def
import           Primitive.Definition.Container ( ContainerData )
-- end def
-- start functionality
import           FunctionDef.Pure.Matcher       ( MatchModel(..) )
import           FunctionDef.Pure.Modifier      ( ReplaceInfoField(..)
                                                , ReplaceField(..)
                                                , Add2Field(..)
                                                )
import           FunctionDef.Pure.Setter        ( StringLike2Primitive
                                                    ( fromString
                                                    )
                                                )
import           FunctionDef.Pure.Transformer   ( Model2IdTuple(toIdTuple) )
-- end functionality

instance Model2IdTuple ContainerData where
    toIdTuple cdata = ("data-container", cdata)
