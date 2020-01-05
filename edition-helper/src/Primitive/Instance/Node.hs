{-|
Module : Node.hs
License : see LICENSE
Description : Node instance
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module Primitive.Instance.Node
    ( Node
    )
where

-- start def
import           Primitive.Definition.Node      ( Node
                                                    ( NodeInt
                                                    , NodeInteger
                                                    , NodeFloat
                                                    , NodeDouble
                                                    , NodeBool
                                                    , NodeText
                                                    , NodeEmpty
                                                    , NodeContainer
                                                    )
                                                )
import           Primitive.Definition.Error     ( NodeError )
-- end def

-- start fn
import           FunctionDef.Setter             ( Data2Node
                                                    ( fromString
                                                    , fromText
                                                    , fromInt
                                                    , fromInteger
                                                    , fromFloat
                                                    , fromDouble
                                                    , fromBool
                                                    , fromEmpty
                                                    , fromContainer
                                                    )
                                                )
-- end fn

-- start utility
import           Data.Text                      ( Text )
-- end utility

instance Data2Node Node where
    fromString    = Right NodeString
    fromText      = Right NodeText
    fromInt       = Right NodeInt
    fromInteger   = Right NodeInteger
    fromFloat     = Right NodeFloat
    fromDouble    = Right NodeDouble
    fromBool      = Right NodeBool
    fromEmpty     = Right NodeEmpty
    fromContainer = Right NodeContainer
