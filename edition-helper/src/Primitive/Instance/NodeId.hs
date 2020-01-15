{-|
Module : NodeId
License : see LICENSE
Description : NodeId primitive implements function definitions
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module Primitive.Instance.NodeId
    ( NodeId
    )
where

-- start def
import           Primitive.Definition.Node      ( NodeId(TextIdCons) )
-- end def

-- start fn
import           FunctionDef.Transformer        ( NodeIdType2Text(toText)
                                                , Model2IdTuple(toIdTuple)
                                                )
import           FunctionDef.Matcher            ( MatchModel(isSame, contains) )
-- end fn

-- start utility
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                , isInfixOf
                                                )
-- end utility

-- start setter

-- end setter

-- start transformer

instance NodeIdType2Text NodeId where
    toText (TextIdCons txt) = txt

instance Model2IdTuple NodeId where
    toIdTuple mid = (pack "id", mid)

-- end transformer

-- start match
instance MatchModel NodeId where
    isSame (TextIdCons txt1) (TextIdCons txt2) = txt1 == txt2
    contains (TextIdCons txt1) (TextIdCons txt2) = txt2 `isInfixOf` txt1

-- end match
