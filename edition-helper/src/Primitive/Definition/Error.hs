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

data IdTupleValueError = FirstValueEmpty String
    | FirstValueError StringValueError
    | SecondValueError StringValueError
    | OtherIdTupleError String
    deriving (Typeable)

instance Show IdTupleValueError where
    show (FirstValueEmpty infostr) =
        "First value of id tuple is empty: " ++ infostr
    show (FirstValueError strerr) =
        "Error in first value of id tuple: " ++ (show strerr)
    show (SecondValueError strerr) =
        "Error in second value of id tuple: " ++ (show strerr)
    show (OtherIdTupleError infostr) | null infostr = "Unknown id tuple error"
                                     | otherwise    = infostr
