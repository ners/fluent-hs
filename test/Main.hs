module Main where

import Data.Attoparsec.Text (parseOnly)
import Data.Syntax.Attoparsec.Text (WrappedParser, getParser_)
import Data.Syntax.Printer.Text (Printer, runPrinter_)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as TextBuilder
import Language.Fluent.Resource (resource)
import System.FilePath.Glob
import Text.Pretty.Simple (pPrint)
import Prelude

parse' :: WrappedParser () a -> Text -> Either String a
parse' p = parseOnly $ getParser_ p

print' :: Printer a b -> b -> Either String Text
print' p = fmap (LazyText.toStrict . TextBuilder.toLazyText) . runPrinter_ p

main :: IO ()
main =
    glob "test/cases/*.ftl" >>= mapM_ \filename -> do
        putStrLn filename
        contents <- Text.readFile filename
        case parse' resource contents of
            Left err -> fail err
            Right ast -> do
                pPrint ast
