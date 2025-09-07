module Main where

import Language.Fluent
import System.FilePath.Glob
import Prelude

main :: IO ()
main = print =<< glob "test/ftl/*.ftl"
