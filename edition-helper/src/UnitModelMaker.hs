module UnitModelMaker where
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
                                                , UnitModel(UnitCons)
                                                , ContainerModel(..)
                                                , ContainerData(..)
                                                )

import qualified ModelIdMaker                  as MiMaker
import qualified ModelInfoMaker                as MinMaker
import qualified ModelTypeMaker                as MtMaker
import qualified ModelAttrMaker                as MaMaker

-- class

class MUnitMaker model where
    fromString :: ModelId -> ModelType -> ModelAttrs -> String -> model
    fromText :: ModelId -> ModelType -> ModelAttrs -> Txt.Text -> model

-- instance

instance MUnitMaker UnitModelMaker where
    
