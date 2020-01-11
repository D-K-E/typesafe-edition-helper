{-|
Module : Node.hs
License : see LICENSE
Description : Node instance
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module Primitive.Instance.Node
where

-- start def
import           Primitive.Definition.Error ( NodeError )
import           Primitive.Definition.Node  ( Container )
import qualified Primitive.Definition.Node  as PN ( PreNode (..) )
import qualified Primitive.Definition.Node  as N ( Node (..) )
-- end def

-- start fn
import qualified FunctionDef.Setter as D2N ( Data2Node (..) )
-- end fn

-- start utility
import Data.Text ( Text )
-- end utility

instance D2N.Data2Node N.Node where
    fromPreNode (PN.PreNodeInt nodeData)     = Right ( N.NodeInt nodeData)
    fromPreNode (PN.PreNodeInteger nodeData) = Right ( N.NodeInteger nodeData)
    fromPreNode (PN.PreNodeFloat nodeData)   = Right ( N.NodeFloat nodeData)
    fromPreNode (PN.PreNodeDouble nodeData)  = Right ( N.NodeDouble nodeData)
    fromPreNode (PN.PreNodeBool nodeData)    = Right ( N.NodeBool nodeData)
    fromPreNode (PN.PreNodeText nodeData)    = Right ( N.NodeText nodeData)
    fromPreNode (PN.PreNodeEmpty nodeData)   = Right ( N.NodeEmpty nodeData)
    fromContainer ncont = Right (N.NodeContainer ncont)
