{-|
Module : NodeInfo Control
License : see LICENSE
Description : ModuleInfo.hs maker
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Pure.NodeInfo
    ()
where


import Control.Pure.NodeAttr
       ( makeNodeAttrFromIdTupleStringMap
       , makeNodeAttrFromIdTupleTextMap
       , makeNodeAttrFromStringMap
       , makeNodeAttrFromTextMap
       )
import Control.Pure.NodeId
       ( makeNodeIdFromIdTuple, makeNodeIdFromString, makeNodeIdFromText )
