module Language.Fluent.Bundle
    ( Bundle (..)
    , FluentValue (..)
    , formatPattern
    )
where

import Data.Text (Text)
import Language.Fluent.Expression (Expression, Pattern)
import Prelude

data Bundle

class FluentValue a where
    format :: Expression -> a -> Text

type FluentArg a = (Text, a)

class FormatPattern r

instance FormatPattern (Either String Text)

instance (FluentValue a, FormatPattern r) => FormatPattern (FluentArg a -> r)

formatPattern :: (FormatPattern r) => Bundle -> Pattern -> r
formatPattern b p = undefined
