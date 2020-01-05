{-|
Module : Control.Pure.NodeAttr.hs
License : see LICENSE
Description : Control functions for NodeAttr type
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Control.Pure.NodeAttr
    ( makeNodeAttrFromStringMap
    , makeNodeAttrFromTextMap
    , makeNodeAttrFromIdTupleStringMap
    , makeNodeAttrFromIdTupleTextMap
    )
where

-- start def
import           Primitive.Definition.NodeAttr ( NodeAttr )
import           Primitive.Instance.NodeAttr   ( NodeAttr )
import           Primitive.Definition.Error     ( MapValueError
                                                    ( MapKeyError
                                                    , MapValError
                                                    , OtherMapError
                                                    )
                                                , IdTupleValueError
                                                    ( SecondMapValueError
                                                    , FirstValueError
                                                    )
                                                , TextValueError
                                                    ( EmptyStr
                                                    , OtherStringError
                                                    )
                                                )
-- end def
-- start fn
import           FunctionDef.Setter             ( Map2Primitive(..)
                                                , TupleMap2Primitive(..)
                                                )
-- end fn
-- start utility
import           Data.Text                      ( Text )
import           Data.Map.Strict                ( elems
                                                , keys
                                                , toList
                                                , filterWithKey
                                                , Map
                                                ) -- importing type
import qualified Data.Map.Strict               as Mp
                                                ( filter )
import           Utils.StrUtils                 ( isAlphaNumStr
                                                , isAsciiStr
                                                )
import           Utils.MapUtils                 ( convertStringMap2Txt
                                                , convertTxtMap2String
                                                )

showMapKeyVal :: Int -> Map String String -> String

showMapKeyVal 0 amap = fst (head (toList amap))
showMapKeyVal 1 amap = snd (head (toList amap))

showMapKey :: Map String String -> String
showMapKey = showMapKeyVal 0
showMapVal :: Map String String -> String
showMapVal = showMapKeyVal 1
showFirstKV :: Map String String -> String
showFirstKV amap =
    "Key: " ++ (showMapKey amap) ++ "\n" ++ "Val: " ++ (showMapVal amap)

-- end utility
makeNodeAttrFromStringMap
    :: Map String String -> Either MapValueError NodeAttr
makeNodeAttrFromStringMap aMap
    | any null (elems aMap)
    = let errstr = showFirstKV (Mp.filter null aMap)
      in  Left (MapValError "Attributes must have non empty values" errstr)
    | any null (keys aMap)
    = let errstr = showFirstKV (filterWithKey filtfn aMap)
                  where filtfn key val = null key
      in  Left (MapKeyError "Attributes must have non empty keys" errstr)
    | not (all isAlphaNumStr (elems aMap))
    = let errstr = showMapVal (Mp.filter isAlphaNumStr aMap)
      in
          Left
              (MapValError
                  "Attributes must have alphanumeric values unlike: "
                  errstr
              )
    | not (all isAsciiStr (elems aMap))
    = let errstr = showMapVal (Mp.filter isAsciiStr aMap)
      in  Left (MapValError "Attributes must have ascii values" errstr)
    | not (all isAlphaNumStr (keys aMap))
    = let errstr = showMapKey (filterWithKey fn aMap)
                  where fn key val = isAlphaNumStr key
      in  Left (MapKeyError "Attributes must have alphanumeric keys" errstr)
    | not (all isAsciiStr (keys aMap))
    = let errstr = showMapKey (filterWithKey fn aMap)
                  where fn key val = isAsciiStr key
      in  Left (MapKeyError "Attributes must have ascii keys" errstr)

makeNodeAttrFromTextMap :: Map Text Text -> Either MapValueError NodeAttr
makeNodeAttrFromTextMap amap =
    makeNodeAttrFromStringMap (convertTxtMap2String amap)

makeNodeAttrFromIdTupleStringMap
    :: (String, Map String String) -> Either IdTupleValueError NodeAttr

makeNodeAttrFromIdTupleStringMap (str, amap)
    | null str
    = Left (FirstValueError (EmptyStr "IdTuple first argument"))
    | not (str == "attribute")
    = Left
        (FirstValueError
            (OtherStringError
                "IdTuple first value is not attribute for NodeAttr id tuple"
            )
        )
    | otherwise
    = let res = makeNodeAttrFromStringMap amap
      in  case res of
              Left  err -> Left (SecondMapValueError err)
              Right m   -> Right m

makeNodeAttrFromIdTupleTextMap
    :: (String, Map Text Text) -> Either IdTupleValueError NodeAttr


makeNodeAttrFromIdTupleTextMap (str, amap) =
    makeNodeAttrFromIdTupleStringMap (str, (convertTxtMap2String amap))
