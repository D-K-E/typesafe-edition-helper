{-|
Module : Model
License : see LICENSE
Description : Container primitive
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}

module Primitive.Instance.Container where

-- definition related imports
import           Primitive.Definition.Container ( CData )
-- end of definition related imports

import           View.Transformer               ( Model2Tuple(..) )

instance Model2Tuple ContainerModel where
    toTuple model = (modelInfo model, CData (modelData model))
