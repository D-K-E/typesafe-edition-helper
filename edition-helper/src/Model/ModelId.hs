{-|
Module : ModelId
License : see LICENSE
Description : ModelId primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module ModelId
    ( ModelId(..)
    )
where
import           Data.Map                       ( Map ) -- importing type
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                ) -- importing type
import           Utils.StrUtils                 ( toLowerString
                                                , isAlphaNumStr
                                                , isAsciiStr
                                                )

import           Utils.DataUtils                ( StringLikeCons )


-- | model id: alphanumeric non empty string has to be unique for each model
data ModelId = TextIdCons Text
                | StringIdCons String
                deriving (Eq, Show)

instance StringLikeCons ModelId where
    fromString aStr
        | null aStr
        = error "empty string is not allowed as id"
        | not (isAlphaNumStr aStr)
        = error "Only ascii alphanumeric strings are allowed"
        | not (isAsciiStr aStr)
        = error "Only ascii alphanumeric strings are allowed"
        | otherwise
        = StringIdCons aStr

instance Model2StringText ModelId where
    toString (StringIdCons aModel) = aModel
    toString (TextIdCons   aModel) = unpack aModel
    toText (TextIdCons   aModel) = aModel
    toText (StringIdCons aModel) = pack aModel
