{-|
Module : ModelInfo Control
License : see LICENSE
Description : ModuleInfo.hs maker
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Pure.ModelInfo
    ()
where


import           Control.Pure.ModelId           ( makeModelIdFromString
                                                , makeModelIdFromText
                                                , makeModelIdFromIdTuple
                                                )
import           Control.Pure.ModelAttr         ( makeModelAttrFromStringMap
                                                , makeModelAttrFromTextMap
                                                , makeModelAttrFromIdTupleStringMap
                                                , makeModelAttrFromIdTupleTextMap
                                                )
