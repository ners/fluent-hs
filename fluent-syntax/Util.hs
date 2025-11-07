module Util where

import Control.Category.Structures ((/+/))
import Control.Lens (Iso', iso)
import Control.Lens.SemiIso (constant)
import Control.Lens.TH (makePrisms)
import Control.SIArrow (SIArrow (sisome), (*/), (/$/))
import Data.Attoparsec.Text (endOfInput, parseOnly)
import Data.Bool qualified as Bool
import Data.Maybe (isJust)
import Data.MonoTraversable (MonoPointed (opoint))
import Data.Syntax (Syntax (takeWhile1))
import Data.Syntax.Attoparsec.Text (WrappedParser, getParser_)
import Data.Syntax.Char (SyntaxChar, endOfLine, spaces1)
import Data.Syntax.Combinator (opt_, optional)
import Data.Syntax.Printer.Text (Printer, runPrinter_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as TextBuilder
import Prelude

textIso :: Iso' Text String
textIso = iso Text.unpack Text.pack

$(makePrisms ''Either)

either :: (SyntaxChar syn) => syn () a -> syn () b -> syn () (Either a b)
either a b = _Left /$/ a /+/ _Right /$/ b

bool :: (SyntaxChar syn) => syn () () -> syn () Bool
bool p = iso (Bool.bool Nothing (Just ())) isJust /$/ optional p

blankInline :: (SyntaxChar syn) => syn () ()
blankInline = constant (opoint ' ') /$/ takeWhile1 (flip (elem @[]) " \t")

blank :: (SyntaxChar syn) => syn () ()
blank = spaces1

blankBlock :: (SyntaxChar syn) => syn () ()
blankBlock = constant [()] /$/ sisome (opt_ blankInline */ endOfLine)

parse :: WrappedParser () a -> Text -> Either String a
parse p = parseOnly $ getParser_ p <* endOfInput

print :: Printer a b -> b -> Either String Text
print p = fmap (LazyText.toStrict . TextBuilder.toLazyText) . runPrinter_ p
