module Main where

import Control.Lens ((^.))
import Data.Attoparsec.Text (endOfInput, parseOnly)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Syntax.Attoparsec.Text (WrappedParser, getParser_)
import Data.Syntax.Printer.Text (Printer, runPrinter_)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as TextBuilder
import Language.Fluent.Bundle
import Language.Fluent.Locale (Locale (Locale))
import Language.Fluent.Message (getValue)
import Language.Fluent.Syntax.Literal (NumberLiteral (NumberLiteral))
import Language.Fluent.Syntax.Resource (resource)
import Language.Fluent.Value (Number (numberType), NumberType (Ordinal), SomeValue (..), numberIso)
import System.FilePath.Glob
import Text.Pretty.Simple (pPrint)
import Prelude

parse' :: WrappedParser () a -> Text -> Either String a
parse' p = parseOnly $ getParser_ p <* endOfInput

print' :: Printer a b -> b -> Either String Text
print' p = fmap (LazyText.toStrict . TextBuilder.toLazyText) . runPrinter_ p

ordinal :: [SomeValue] -> Map Text SomeValue -> Either String SomeValue
ordinal [NumberValue n] (Map.null -> True) = Right $ NumberValue n{numberType = Ordinal}
ordinal _ _ = Left "ordinal: unexpected parameters"

main :: IO ()
main = do
    glob "test/cases/*.ftl" >>= mapM_ \filename -> do
        putStrLn filename
        contents <- Text.readFile filename
        case parse' resource contents of
            Left err -> fail err
            Right ast -> do
                pPrint ast

    photos <- Text.readFile "test/cases/shared-photos.ftl"
    let r = either error id $ parse' resource photos
    let bundle = buildBundle [Locale "en"] [r] & addFunction "ORDINAL" ordinal

    let msg = fromMaybe (error "Failed to get message") $ getMessage bundle "hello-user"
    let pattern = fromMaybe (error "Failed to get value") $ getValue msg
    either error Text.putStrLn $ formatPattern bundle pattern ("userName" :: Text, "ners" :: Text)

    let scope1 =
            Map.fromList @Text @SomeValue
                [ ("userName", StringValue "ners")
                , ("userGender", StringValue "male")
                , ("photoCount", NumberValue $ NumberLiteral "1" ^. numberIso)
                ]
    let scope2 = Map.insert "photoCount" (NumberValue $ NumberLiteral "2" ^. numberIso) scope1
    let msg = fromMaybe (error "Failed to get message") $ getMessage bundle "shared-photos"
    let pattern = fromMaybe (error "Failed to get value") $ getValue msg
    either error Text.putStrLn $ formatPattern bundle pattern scope1
    either error Text.putStrLn $ formatPattern bundle pattern scope2
    let msg = fromMaybe (error "Failed to get message") $ getMessage bundle "nth-photo"
    let pattern = fromMaybe (error "Failed to get value") $ getValue msg
    either error Text.putStrLn $ formatPattern bundle pattern scope1
    either error Text.putStrLn $ formatPattern bundle pattern scope2
