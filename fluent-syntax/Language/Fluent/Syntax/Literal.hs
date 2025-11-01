module Language.Fluent.Syntax.Literal where

import Control.Lens (iso)
import Control.Lens.SemiIso (ASemiIso', semiIso)
import Control.Lens.TH (makePrisms)
import Control.SIArrow (SIArrow (sisome), (*/), (/$/), (/$~), (/*), (/*/))
import Data.Attoparsec.Text (parseOnly)
import Data.Char (isDigit)
import Data.Syntax (Syntax (char, satisfy))
import Data.Syntax.Attoparsec.Text (getParser_)
import Data.Syntax.Char (SyntaxChar)
import Data.Syntax.Combinator (optional)
import Data.Text (Text)
import Data.Text qualified as Text
import Util (either, textIso)
import Prelude

newtype NumberLiteral = NumberLiteral Text
    deriving stock (Eq, Show)

newtype StringLiteral = StringLiteral Text
    deriving stock (Eq, Show)

type Literal = Either NumberLiteral StringLiteral

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

stringLiteral :: (SyntaxChar syn) => syn () StringLiteral
stringLiteral = _StringLiteral . textIso /$/ char '"' */ sisome quotedChar /* char '"'

quotedChar :: (SyntaxChar syn) => syn () Char
quotedChar = satisfy (`notElem` ("\"\\\r\n" :: String))

literal :: (SyntaxChar syn) => syn () Literal
literal = Util.either numberLiteral stringLiteral
