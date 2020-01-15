{-|
Module : Model
License : see LICENSE
Description : Make project layout
Copyright : Kaan Eraslan
Maintainer : Kaan Eraslan
Stability : Experimental
-}
module Io.ProjectLayout where

import Control.Monad    ( mapM )
import System.Directory ( createDirectory )
import System.FilePath  ( FilePath (..), addExtension, combine )
import System.IO

--makeDirs :: [String] -> [IO ()]
