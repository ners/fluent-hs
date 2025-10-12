module Language.Fluent.Syntax.Identifier where

import Control.Lens (Cons (_Cons), iso)
import Control.Lens.TH (makePrisms)
import Control.SIArrow (SIArrow (simany), (/$/), (/*/))
import Data.Char (isAscii, isLetter, isNumber)
import Data.Syntax (Syntax (satisfy))
import Data.Syntax.Char (SyntaxChar)
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude

newtype Identifier = Identifier Text
    deriving newtype (Show, Eq)

makePrisms ''Identifier

identifier :: (SyntaxChar syn) => syn () Identifier
identifier =
    (_Identifier . iso Text.unpack Text.pack . _Cons)
        /$/ satisfy isAsciiLetter
        /*/ simany (satisfy isValid)
  where
    isAsciiLetter c = isAscii c && isLetter c
    isValid c = isAscii c && (isLetter c || isNumber c) || c == '-' || c == '_'
