{-|
Module : Control.Node.hs
License : see LICENSE
Description : Control functions for Node type
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Node where

-- start def
import           Primitive.Definition.Error     ( NodeError(..) )
import           Primitive.Definition.Node      ( Node
                                                    ( NodeBool
                                                    , NodeContainer
                                                    , NodeDouble
                                                    , NodeEmpty
                                                    , NodeFloat
                                                    , NodeInt
                                                    , NodeInteger
                                                    , NodeText
                                                    )
                                                , PreNode
                                                    ( PreNodeBool
                                                    , PreNodeDouble
                                                    , PreNodeEmpty
                                                    , PreNodeFloat
                                                    , PreNodeInt
                                                    , PreNodeInteger
                                                    , PreNodeText
                                                    )
                                                )
import           Primitive.Instance.Node
-- end def
-- start fn
import           FunctionDef.Setter             ( Data2Node(..)
                                                , IdTuple2Node(..)
                                                )
-- end fn
-- start utility
import           Data.Text                      ( Text
                                                , empty
                                                , pack
                                                , unpack
                                                )
-- end utility

makePreNode str = PreNodeInt (read str :: Int)
makePreNode str = PreNodeInteger (read str :: Integer)
makePreNode str = PreNodeFloat (read str :: Float)
makePreNode str = PreNodeDouble (read str :: Double)
makePreNode str = PreNodeText (pack str)
makePreNode str = PreNodeBool (read str :: Bool)

makeNodeFromPreNode :: PreNode -> Either NodeError Node
makeNodeFromPreNode (PreNodeInt     pint) = Right (NodeInt pint)
makeNodeFromPreNode (PreNodeInteger pint) = Right (NodeInteger pint)
makeNodeFromPreNode (PreNodeFloat   pf  ) = Right (NodeFloat pf)
makeNodeFromPreNode (PreNodeDouble  pd  ) = Right (NodeDouble pd)
makeNodeFromPreNode (PreNodeBool    pb  ) = Right (NodeBool pb)

makeNodeFromIdTupleString :: (String, String) -> Either NodeError Node
makeNodeFromIdTupleString (str1, str2)
    | str1 == "int"
    = makeNodeFromPreNode (PreNodeInt (read str2 :: Int))
    | str1 == "integer"
    = makeNodeFromPreNode (PreNodeInteger (read str2 :: Integer))
    | str1 == "float"
    = makeNodeFromPreNode (PreNodeFloat (read str2 :: Float))
    | str1 == "double"
    = makeNodeFromPreNode (PreNodeDouble (read str2 :: Double))
    | str1 == "text"
    = makeNodeFromPreNode (PreNodeText (pack str2))
    | str1 == "bool"
    = makeNodeFromPreNode (PreNodeBool (read str2 :: Bool))
