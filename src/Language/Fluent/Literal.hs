module Language.Fluent.Literal where

import Control.Lens (iso)
import Control.Lens.SemiIso (ASemiIso', semiIso)
import Control.Lens.TH (makePrisms)
import Control.SIArrow (SIArrow (sisome), (/$/), (/$~), (/*/))
import Data.Attoparsec.Text (parseOnly)
import Data.Char (isDigit)
import Data.Syntax (Syntax (char, satisfy, string))
import Data.Syntax.Attoparsec.Text (getParser_)
import Data.Syntax.Char (SyntaxChar)
import Data.Syntax.Combinator (optional)
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude

newtype NumberLiteral = NumberLiteral Text
    deriving stock (Show)

newtype StringLiteral = StringLiteral Text
    deriving stock (Show)

makePrisms ''NumberLiteral

makePrisms ''StringLiteral

numberLiteral :: (SyntaxChar syn) => syn () NumberLiteral
numberLiteral = (_NumberLiteral . foo) /$~ bar
  where
    bar :: (SyntaxChar syn) => syn () ((Maybe (), Text), Maybe ((), Text))
    bar = optional (char '-') /*/ digits /*/ optional (char '.' /*/ digits)
    foo :: ASemiIso' Text ((Maybe (), Text), Maybe ((), Text))
    foo = semiIso (parseOnly (getParser_ bar)) (\((neg, whole), dec) -> Right $ maybe "" (const "-") neg <> whole <> maybe "" snd dec)
    digits :: (SyntaxChar syn) => syn () Text
    digits = iso Text.unpack Text.pack /$/ sisome (satisfy isDigit)
