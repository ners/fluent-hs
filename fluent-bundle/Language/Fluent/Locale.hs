module Language.Fluent.Locale where

import Data.String (IsString)
import Data.Text (Text)
import Prelude

newtype Locale = Locale Text
    deriving newtype (Eq, Show, IsString)
