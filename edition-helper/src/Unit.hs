module Unit
    ( UnitModel
    )
where

-- unit has an id, data, and type
import Data.Map(Map)  -- importing type
import qualified Data.Map as Dict  -- importing module

-- Model Declaration

data UnitProperty = UProp {
     key :: String
    ,val :: String
    } deriving (Show, Eq)


data UnitModel = UnitModel {
     unitId :: String
    ,unitType :: String
    ,unitProps :: Map
    ,unitData :: String
    } deriving (Show, Eq)

-- End of Model Declaration
