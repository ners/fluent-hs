module Language.Fluent.Message where

import Data.List (find)
import Data.Text (Text)
import Language.Fluent.Syntax.Expression (Pattern)
import Language.Fluent.Syntax.Expression qualified as Syntax
import Language.Fluent.Syntax.Identifier (Identifier (Identifier))
import Language.Fluent.Syntax.Message qualified as Syntax
import Prelude

newtype Attribute = Attribute Syntax.Attribute

newtype Message = Message Syntax.Message

getValue :: Message -> Maybe Pattern
getValue (Message Syntax.Message{value}) = value

getAllAttributes :: Message -> [Attribute]
getAllAttributes (Message Syntax.Message{attributes}) = Attribute <$> attributes

getAttribute :: Text -> Message -> Maybe Attribute
getAttribute (Identifier -> identifier) =
    find (\(Attribute (Syntax.Attribute i _)) -> i == identifier) . getAllAttributes
