module Language.Fluent.Entry where

import Control.Lens (makePrisms)
import Control.SIArrow (SIArrow (simany), (*/), (/$/), (/$~), (/*), (/*/))
import Data.Syntax (Syntax (char))
import Data.Syntax.Char (SyntaxChar, endOfLine)
import Data.Syntax.Combinator (choice, opt)
import Language.Fluent.Comment (Comment, comment)
import Language.Fluent.Expression (Attribute, Pattern, attribute, pattern_)
import Language.Fluent.Identifier (Identifier, identifier)
import Language.Fluent.Message (Message, message)
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
