module Language.Fluent.Message where

import Control.Lens.TH (makePrisms)
import Language.Fluent.Attribute
import Language.Fluent.Comment
import Language.Fluent.Expression
import Language.Fluent.Identifier
import Prelude

data Message = Message
    { id :: Identifier
    , value :: Maybe Pattern
    , attributes :: [Attribute]
    , comment :: Maybe Comment
    }

makePrisms ''Message
