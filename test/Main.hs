module Main where

import Data.Attoparsec.Text (endOfInput, parseOnly)
import Data.Either (fromRight)
import Data.Maybe (fromJust, fromMaybe)
import Data.Syntax.Attoparsec.Text (WrappedParser, getParser_)
import Data.Syntax.Printer.Text (Printer, runPrinter_)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as TextBuilder
import Language.Fluent.Bundle
import Language.Fluent.Locale (Locale (Locale))
import Language.Fluent.Message (getValue)
import Language.Fluent.Syntax.Resource (resource)
import System.FilePath.Glob
import Text.Pretty.Simple (pPrint)
import Prelude

parse' :: WrappedParser () a -> Text -> Either String a
parse' p = parseOnly $ getParser_ p <* endOfInput

print' :: Printer a b -> b -> Either String Text
print' p = fmap (LazyText.toStrict . TextBuilder.toLazyText) . runPrinter_ p

main :: IO ()
main = do
    let r = either error id $ parse' resource "hello = Hello, {$name}!"
    let bundle = buildBundle [Locale "en"] [r]
    let msg = fromMaybe (error "Failed to get message") $ getMessage bundle "hello"
    let pattern = fromMaybe (error "Failed to get value") $ getValue msg
    either error Text.putStrLn $ formatPattern bundle pattern ("name" :: Text, "Alex" :: Text)

-- glob "test/cases/*.ftl" >>= mapM_ \filename -> do
--     putStrLn filename
--     contents <- Text.readFile filename
--     case parse' resource contents of
--         Left err -> fail err
--         Right ast -> do
--             pPrint ast
