module ModelMaker where
-- model maker for making models from set of parameters
import           Data.Map                       ( Map )  -- importing type
import qualified Data.Map                      as Dict   -- importing type
import qualified Data.Text                     as Txt
                                                ( Text
                                                , pack
                                                , unpack
                                                )
import           Utils                          ( toLowerString
                                                , isAlphaNumStr
                                                , isAsciiStr
                                                , convertStringKey
                                                , convertStringVal
                                                , convertStringMap2Txt
                                                , convertTxtMap2String
                                                , convertTxtKey
                                                , convertTxtVal
                                                )
import           Model                          ( ModelType
                                                , ModelId(..)
                                                , ModelAttrs(..)
                                                )
