module Language.Fluent.Syntax.Message where

import Control.Lens (iso)
import Control.Lens.TH (makePrisms)
import Control.SIArrow (SIArrow (simany, sisome), (/$/), (/*), (/*/))
import Data.Syntax (Syntax (char))
import Data.Syntax.Char (SyntaxChar)
import Data.Syntax.Combinator (opt)
import Language.Fluent.Syntax.Comment
import Language.Fluent.Syntax.Expression
import Language.Fluent.Syntax.Identifier
import Util (blankInline, either)
import Prelude

data Message = Message
    { id :: Identifier
    , value :: Maybe Pattern
    , attributes :: [Attribute]
    , comment :: Maybe Comment
    }
    deriving stock (Show)

makePrisms ''Message

message :: (SyntaxChar syn) => syn () Message
message = iso fromMessage toMessage /$/ (identifier /* opt blankInline /* char '=' /* opt blankInline) /*/ Util.either (pattern_ /*/ simany attribute) (sisome attribute)
  where
    fromMessage :: Message -> (Identifier, Either (Pattern, [Attribute]) [Attribute])
    fromMessage Message{value = Just value, ..} = (id, Left (value, attributes))
    fromMessage Message{value = Nothing, ..} = (id, Right attributes)

    toMessage :: (Identifier, Either (Pattern, [Attribute]) [Attribute]) -> Message
    toMessage (id, Left (Just -> value, attributes)) = Message{comment = Nothing, ..}
    toMessage (id, Right attributes) = Message{value = Nothing, comment = Nothing, ..}
