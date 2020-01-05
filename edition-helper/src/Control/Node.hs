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
import           Primitive.Definition.Node      ( Node
                                                    ( NodeInt
                                                    , NodeInteger
                                                    , NodeFloat
                                                    , NodeDouble
                                                    , NodeString
                                                    , NodeBool
                                                    , NodeText
                                                    , NodeEmpty
                                                    , NodeContainer
                                                    )
                                                , PreNode
                                                    ( PreNodeInt
                                                    , PreNodeInteger
                                                    , PreNodeFloat
                                                    , PreNodeDouble
                                                    , PreNodeString
                                                    , PreNodeText
                                                    , PreNodeBool
                                                    , PreNodeEmpty
                                                    )
                                                )
import           Primitive.Instance.Node
import           Primitive.Definition.Error     ( NodeError(..) )
-- end def
-- start fn
import           FunctionDef.Setter             ( Data2Node(..)
                                                , IdTuple2Node(..)
                                                )
-- end fn
-- start utility
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                , empty
                                                )
-- end utility

makePreNode str            = PreNodeInt (read str :: Int)
makePreNode str            = PreNodeInteger (read str :: Integer)
makePreNode str            = PreNodeFloat (read str :: Float)
makePreNode str            = PreNodeDouble (read str :: Double)
makePreNode str            = PreNodeString str
makePreNode str            = PreNodeText (pack str)
makePreNode str            = PreNodeBool (read str :: Bool)
makePreNode str | null str = PreNodeNothing Nothing

makeNodeFromPreNode :: PreNode -> Either NodeError Node
makeNodeFromPreNode (PreNodeInt     pint) = fromInt pint
makeNodeFromPreNode (PreNodeInteger pint) = fromInteger pint
makeNodeFromPreNode (PreNodeFloat   pf  ) = fromFloat pf
makeNodeFromPreNode (PreNodeDouble  pd  ) = fromDouble pf
makeNodeFromPreNode (PreNodeBool    pb  ) = fromBool pf
makeNodeFromPreNode (PreNodeString  str ) = fromString pf
makeNodeFromPreNode (PreNodeNothing nth ) = fromEmpty nth

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
    | str1 == "empty" `and` null str2
    = makeNodeFromPreNode (PreNodeEmpty Nothing)
