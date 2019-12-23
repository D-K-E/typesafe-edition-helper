{-|
Module : Model
License : see LICENSE
Description : ModelId.hs monadic maker
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Impure.ModelId
    ( makeModelIdM
    )
where

-- start def
import           Primitive.Instance.Impure.ModelId
                                                ( ModelId )
import           Primitive.Definition.Error     ( StringValueError(..) )
-- end def
import           FunctionDef.Pure.Setter        ( StringLike2Primitive
                                                    ( fromString
                                                    )
                                                )
import           FunctionDef.Impure.Setter      ( StringLike2PrimitiveM
                                                    ( fromStringM
                                                    )
                                                )
import           Control.Exception              ( throw )
import           Utils.StrUtils                 ( isAlphaNumStr
                                                , isAsciiStr
                                                )


makeModelIdM :: String -> Either StringValueError ModelId
makeModelIdM astr
    | null astr = Left (EmptyStr "ModelId")
    | not ((isAlphaNumStr astr) && (isAsciiStr astr)) = Left
        (NotAsciiAlphanumeric "ModelId")
    | otherwise = Right (fromString astr)
