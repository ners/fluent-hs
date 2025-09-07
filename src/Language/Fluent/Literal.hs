module Language.Fluent.Literal where

import Control.Category.Structures ((/+/))
import Control.Lens (Iso', iso)
import Control.Lens.SemiIso (ASemiIso', rev, semiIso)
import Control.Lens.TH (makePrisms)
import Control.SIArrow (SIArrow (sisome), (*/), (/$/), (/$~), (/*), (/*/))
import Data.Attoparsec.Text (parseOnly)
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.Syntax (Syntax (char, satisfy, string), packed)
import Data.Syntax.Attoparsec.Text (getParser_)
import Data.Syntax.Char (SyntaxText, spaces, spaces_)
import Data.Syntax.Combinator (optional, sepBy, sepBy1)
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Fluent.Attribute
import Language.Fluent.Comment
import Language.Fluent.Identifier
import Prelude

newtype NumberLiteral = NumberLiteral Text

newtype StringLiteral = StringLiteral Text

makePrisms ''NumberLiteral

makePrisms ''StringLiteral

numberLiteral :: (SyntaxText syn) => syn () NumberLiteral
numberLiteral = (_NumberLiteral . foo) /$~ bar
  where
    bar :: (SyntaxText syn) => syn () ((Maybe (), Text), Maybe ((), Text))
    bar = optional (string "-") /*/ digits /*/ optional (string "." /*/ digits)
    foo :: ASemiIso' Text ((Maybe (), Text), Maybe ((), Text))
    foo = semiIso (parseOnly (getParser_ bar)) (\((neg, whole), dec) -> Right $ maybe "" (const "-") neg <> whole <> maybe "" snd dec)
    digits :: (SyntaxText syn) => syn () Text
    digits = iso Text.unpack Text.pack /$/ sisome (satisfy isDigit)
