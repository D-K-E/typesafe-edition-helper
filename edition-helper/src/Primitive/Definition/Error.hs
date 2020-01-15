{-|
Module : Error
License : see LICENSE
Description : Custom error definitions
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Primitive.Definition.Error
    ( TextValueError(..)
    , IdTupleValueError(..)
    , MapValueError(..)
    , NodeError(..)
    )
where

import qualified Control.Exception as Ex
import           Data.Text         ( Text, unpack )
import           Type.Reflection   ( Typeable )
import           Utils.StrUtils    ( concatHStr, concatTStr )

-- |'TextValueError' defines errors for string
data TextValueError = EmptyText Text -- string is empty
    | NotAscii Text
    | NotAlphanumeric Text
    | NotAsciiAlphanumeric Text
    | OtherTextError Text
    deriving (Typeable)

instance Show TextValueError where
    show (EmptyText infostr) = "String value is empty for " ++ unpack infostr
    show (NotAscii infostr) =
        "String value is not entirely composed of ASCII characters for "
            ++ unpack infostr
    show (NotAlphanumeric infostr) =
        "String value is not entirely composed of alphanumeric characters for "
            ++ unpack infostr
    show (NotAsciiAlphanumeric infostr) =
        "String not entirely composed of alphanumeric ASCII characters for "
            ++ unpack infostr
    show (OtherTextError str) | null (unpack str) = "Unknown string error"
                                | otherwise         = unpack str

instance Ex.Exception TextValueError

-- |'MapValueError' regroups map errors
data MapValueError = MapKeyError Text Text
    | MapValError Text Text
    | OtherMapError Text
    deriving (Typeable)


instance Show MapValueError where
    show (MapKeyError mess str) =
        "Map key error: " ++ unpack mess ++ " for key: " ++ unpack str
    show (MapValError mess str) =
        "Map value error: " ++ unpack mess ++ " for value: " ++ unpack str
    show (OtherMapError mess) = "Map error: " ++ unpack mess


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

makeNodeErrString :: String -> String -> String
makeNodeErrString tname mess =
    "Node"
        ++ tname
        ++ "Error: in constructing node from"
        ++ tname
        ++ ": "
        ++ mess

instance Show NodeError where
    show (NodeIntError       mess) = makeNodeErrString "Int" mess
    show (NodeIntegerError   mess) = makeNodeErrString "Integer" mess
    show (NodeFloatError     mess) = makeNodeErrString "Float" mess
    show (NodeDoubleError    mess) = makeNodeErrString "Double" mess
    show (NodeStringError    mess) = makeNodeErrString "String" mess
    show (NodeBoolError      mess) = makeNodeErrString "Bool" mess
    show (NodeTextError      mess) = makeNodeErrString "Text" mess
    show (NodeEmptyError     mess) = makeNodeErrString "Error" mess
    show (NodeContainerError mess) = makeNodeErrString "Container" mess


newtype NodeIdError = NodeIdError TextValueError
newtype NodeTypeError = NodeTypeError TextValueError
newtype NodeAttrError = NodeAttrError MapValueError

instance Show NodeIdError where
    show (NodeIdError serr) = "Node id error: " ++ show serr

instance Show NodeTypeError where
    show (NodeTypeError serr) = "Node type error: " ++ show serr

instance Show NodeAttrError where
    show (NodeAttrError serr) = "Node attribute error: " ++ show serr


data NodeInfoError = NodeInfoIdError NodeIdError
    | NodeInfoTypeError NodeTypeError
    | NodeInfoAttrError NodeAttrError
    | OtherNodeInfoError String
    deriving (Typeable)


instance Show NodeInfoError where
    show (NodeInfoIdError        serr) = "NodeInfo: " ++ show serr
    show (NodeInfoTypeError      serr) = "NodeInfo: " ++ show serr
    show (NodeInfoAttrError      serr) = "NodeInfo: " ++ show serr
    show (OtherNodeInfoError serr)     = "Other NodeInfo error: " ++ serr


data IdTupleValueError = FirstValueEmpty String
    | FirstValueError TextValueError
    | SecondTextValueError TextValueError
    | SecondMapValueError MapValueError
    | OtherIdTupleError String
    deriving (Typeable)


instance Show IdTupleValueError where
    show (FirstValueEmpty infostr) =
        "First value of id tuple is empty: " ++ infostr
    show (FirstValueError strerr) =
        "Error in first value of id tuple: " ++ show strerr
    show (SecondTextValueError strerr) =
        "Error in second string value of id tuple: " ++ show strerr
    show (SecondMapValueError strerr) =
        "Error in second map value of id tuple: " ++ show strerr
    show (OtherIdTupleError infostr) | null infostr = "Unknown id tuple error"
                                     | otherwise    = infostr
