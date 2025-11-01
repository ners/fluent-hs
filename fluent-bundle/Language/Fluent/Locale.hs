module Language.Fluent.Locale where

import Data.Text (Text)
import Prelude

newtype Locale = Locale Text
    deriving newtype (Eq)
