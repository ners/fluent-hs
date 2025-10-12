module Language.Fluent.Value where

import Data.Text (Text)
import Data.Text qualified as Text
import Language.Fluent.Locale (Locale)
import Prelude

class Value a where
    format :: [Locale] -> a -> Either String Text

data SomeValue
    = StringValue Text
    | NumberValue Text
    | SomeValue CustomValue

data CustomValue = forall a. (Value a) => CustomValue a

instance Value SomeValue where
    format locales (StringValue s) = format locales s
    format locales (NumberValue n) = format locales n
    format locales (SomeValue a) = format locales a

instance Value CustomValue where
    format locales (CustomValue a) = format locales a

instance Value Text where format _ = Right . id

instance Value String where format locales = format locales . Text.pack
