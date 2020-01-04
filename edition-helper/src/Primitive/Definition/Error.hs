{-|
Module : Error
License : see LICENSE
Description : Custom error definitions
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Definition.Error
    ( StringValueError(..)
    , IdTupleValueError(..)
    , MapValueError(..)
    , NodeError(..)
    )
where

import qualified Control.Exception             as Ex
import           Type.Reflection                ( Typeable )

-- |'StringValueError' defines errors for string
data StringValueError = EmptyStr String -- string is empty
    | NotAscii String
    | NotAlphanumeric String
    | NotAsciiAlphanumeric String
    | OtherStringError String
    deriving (Typeable)

instance Show StringValueError where
    show (EmptyStr infostr) = "String value is empty for " ++ infostr
    show (NotAscii infostr) =
        "String value is not entirely composed of ASCII characters for "
            ++ infostr
    show (NotAlphanumeric infostr) =
        "String value is not entirely composed of alphanumeric characters for "
            ++ infostr
    show (NotAsciiAlphanumeric infostr) =
        "String not entirely composed of alphanumeric ASCII characters for "
            ++ infostr
    show (OtherStringError str) | null str  = "Unknown string error"
                                | otherwise = str

instance Ex.Exception StringValueError

-- |'MapValueError' regroups map errors
data MapValueError = MapKeyError String String
    | MapValError String String
    | OtherMapError String
    deriving (Typeable)


instance Show MapValueError where
    show (MapKeyError mess str) =
        "Map key error: " ++ mess ++ " for key: " ++ str
    show (MapValError mess str) =
        "Map value error: " ++ mess ++ " for value: " ++ str
    show (OtherMapError mess) = "Map error: " ++ mess


data NodeError = NodeIntError String
    | NodeIntegerError String
    | NodeFloatError String
    | NodeDoubleError String
    | NodeStringError String
    | NodeBoolError String
    | NodeTextError String
    | NodeEmptyError String
    | NodeContainerError String
    deriving (Typeable)

makeNodeErrStrinig :: String -> String -> String
makeErrStrinig tname mess =
    "Node"
        ++ tname
        ++ "Error: in constructing node from"
        ++ tname
        ++ ": "
        ++ mess

instance Show NodeError where
    show (NodeIntError       mess) = makeNodeErrStrinig "Int" mess
    show (NodeIntegerError   mess) = makeNodeErrStrinig "Integer" mess
    show (NodeFloatError     mess) = makeNodeErrStrinig "Float" mess
    show (NodeDoubleError    mess) = makeNodeErrStrinig "Double" mess
    show (NodeStringError    mess) = makeNodeErrStrinig "String" mess
    show (NodeBoolError      mess) = makeNodeErrStrinig "Bool" mess
    show (NodeTextError      mess) = makeNodeErrStrinig "Text" mess
    show (NodeEmptyError     mess) = makeNodeErrStrinig "Error" mess
    show (NodeContainerError mess) = makeNodeErrStrinig "Container" mess


data IdTupleValueError = FirstValueEmpty String
    | FirstValueError StringValueError
    | SecondStringValueError StringValueError
    | SecondMapValueError MapValueError
    | OtherIdTupleError String
    deriving (Typeable)


instance Show IdTupleValueError where
    show (FirstValueEmpty infostr) =
        "First value of id tuple is empty: " ++ infostr
    show (FirstValueError strerr) =
        "Error in first value of id tuple: " ++ show strerr
    show (SecondStringValueError strerr) =
        "Error in second string value of id tuple: " ++ show strerr
    show (SecondMapValueError strerr) =
        "Error in second map value of id tuple: " ++ show strerr
    show (OtherIdTupleError infostr) | null infostr = "Unknown id tuple error"
                                     | otherwise    = infostr
