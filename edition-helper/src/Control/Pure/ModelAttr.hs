{-|
Module : Control.Pure.ModelAttr.hs
License : see LICENSE
Description : Control functions for ModelAttr type
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Pure.ModelAttr where

-- start def
import           Primitive.Definition.ModelAttr ( ModelAttr )
import           Primitive.Instance.ModelAttr   ( ModelAttr )
import           Primitive.Definition.Error     ( MapValueError
                                                    ( MapKeyError
                                                    , MapValError
                                                    , OtherMapError
                                                    )
                                                )
-- end def
-- start fn
import           FunctionDef.Setter             ( Map2Primitive(..)
                                                , TupleMap2Primitive(..)
                                                )
-- end fn
-- start utility
import           Data.Map.Strict                ( elems
                                                , keys
                                                , Map
                                                , filter
                                                ) -- importing type
import           Utils.StrUtils                 ( isAlphaNumStr
                                                , isAsciiStr
                                                )
import           Utils.MapUtils                 ( convertStringMap2Txt
                                                , convertTxtMap2String
                                                )

-- end utility
makeModelMapFromStringMap :: Map String String -> Either MapValueError ModelAttr
makeModelMapFromStringMap aMap
    | any null (elems aMap) = MapValError
        "Attributes must have non empty values"
        (filter any (null (elems aMap)))
    | not (all isAlphaNumStr (elems aMap)) = MapValError
        "Attributes must have alphanumeric values unlike: "
        (filter isAlphaNumStr aMap)
    | not (all isAsciiStr (elems aMap)) = MapValError
        "Attributes must have ascii values"
        (filter isAsciiStr aMap)
    | not (all isAlphaNumStr (keys aMap)) = MapKeyError
        "Attributes must have alphanumeric keys"
        (filter isAlphaNumStr (keys aMap))
    | not (all isAsciiStr (keys aMap)) = MapKeyError
        "Attributes must have ascii keys"
        (filter isAsciiStr (keys aMap))
    | any null (keys aMap) = MapKeyError
        "Attributes must have non empty keys"
        (filter any (null (keys aMap)))
