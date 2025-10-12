module Language.Fluent.Syntax.Entry where

import Control.Lens (makePrisms)
import Control.SIArrow (SIArrow (simany), (*/), (/$/), (/$~), (/*), (/*/))
import Data.Syntax (Syntax (char))
import Data.Syntax.Char (SyntaxChar, endOfLine)
import Data.Syntax.Combinator (choice, opt)
import Language.Fluent.Syntax.Comment (Comment, comment)
import Language.Fluent.Syntax.Expression (Attribute, Pattern, attribute, pattern_)
import Language.Fluent.Syntax.Identifier (Identifier, identifier)
import Language.Fluent.Syntax.Message (Message, message)
import Util (blankInline)
import Prelude

data Entry
    = MessageEntry Message
    | TermEntry Identifier Pattern [Attribute]
    | CommentEntry Comment
    deriving stock (Show)

makePrisms ''Entry

entry :: (SyntaxChar syn) => syn () Entry
entry =
    choice
        [ _MessageEntry /$/ message
        , _TermEntry
            /$~ char '-'
            */ identifier
            /*/ (opt blankInline */ char '=' */ opt blankInline */ pattern_)
            /*/ simany attribute
        , _CommentEntry /$/ comment /* endOfLine
        ]
