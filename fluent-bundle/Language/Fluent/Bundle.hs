module Language.Fluent.Bundle
    ( Bundle (..)
    , buildBundle
    , addFunction
    , getMessage
    , getTerm
    , formatPattern
    )
where

import Control.Applicative ((<|>))
import Control.Lens (filtered, firstOf, to, traversed, view, (^.), _1)
import Control.Monad (forM, (<=<))
import Control.Monad.Extra (mconcatMapM)
import Data.Either.Extra (maybeToEither)
import Data.Fixed (Pico)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Fluent.Locale (Locale)
import Language.Fluent.Message (Attribute (..), Message (Message), getAttribute, getValue)
import Language.Fluent.Syntax.Entry (_MessageEntry, _TermEntry)
import Language.Fluent.Syntax.Expression (CallArguments (..), Expression (..), InlineExpression (..), Pattern (Pattern), PatternElement (..), Variant (..), VariantKey (..), VariantList (..))
import Language.Fluent.Syntax.Expression qualified as Syntax
import Language.Fluent.Syntax.Identifier (Identifier (Identifier))
import Language.Fluent.Syntax.Literal (Literal, StringLiteral (StringLiteral))
import Language.Fluent.Syntax.Message qualified as Syntax
import Language.Fluent.Syntax.Resource (Resource (entries))
import Language.Fluent.Value (CustomValue (CustomValue), Number (..), NumberType (..), SomeValue (NumberValue, SomeValue, StringValue), Value (format), numberIso)
import Language.Fluent.Value qualified as Value
import Prelude

data Bundle = Bundle
    { locales :: [Locale]
    , resources :: [Resource]
    , useIsolating :: Bool
    -- ^ Whether to use Unicode isolation marks (FSI, PDI) for bidi interpolations.
    , functions :: Map Text ([SomeValue] -> Map Text SomeValue -> Either String SomeValue)
    -- ^ Additional functions available to translations as builtins.
    }

buildBundle :: [Locale] -> [Resource] -> Bundle
buildBundle locales resources = Bundle{useIsolating = True, functions = mempty, ..}

addFunction :: Text -> ([SomeValue] -> Map Text SomeValue -> Either String SomeValue) -> Bundle -> Bundle
addFunction k v bundle = bundle{functions = Map.insert k v $ functions bundle}

getMessage :: Bundle -> Text -> Maybe Message
getMessage Bundle{..} (Identifier -> identifier) =
    firstOf
        ( traversed
            . to entries
            . traversed
            . _MessageEntry
            . filtered ((identifier ==) . Syntax.id)
            . to Message
        )
        resources

getTerm :: Bundle -> Text -> Maybe (Pattern, [Attribute])
getTerm Bundle{..} (Identifier -> identifier) =
    firstOf
        ( traversed
            . to entries
            . traversed
            . _TermEntry
            . filtered ((identifier ==) . view _1)
            . to \(_, pattern, attrs) -> (pattern, Attribute <$> attrs)
        )
        resources

type FluentArg a = (Text, a)

class FormatPattern r where
    formatPattern' :: Bundle -> Map Text SomeValue -> Pattern -> r

directionalIsolation :: Text -> Text
directionalIsolation t = "\x2068" <> t <> "\x2069"

instance FormatPattern (Either String Text) where
    formatPattern' :: Bundle -> Map Text SomeValue -> Pattern -> Either String Text
    formatPattern' bundle@Bundle{..} scope (Pattern elements) = mconcatMapM formatPatternElement elements
      where
        resolvePattern :: Bundle -> Map Text SomeValue -> Pattern -> Either String SomeValue
        resolvePattern bundle scope pattern = StringValue <$> formatPattern' bundle scope pattern
        formatPatternElement :: PatternElement -> Either String Text
        formatPatternElement (TextElement t) = format locales t
        formatPatternElement (PlaceablePattern expr)
            | useIsolating
            , length elements > 1
            , case expr of
                Inline MessageReference{} -> True
                Inline TermReference{} -> True
                Inline StringLiteralExpression{} -> True
                _ -> False =
                directionalIsolation <$> f
            | otherwise = f
          where
            f = formatExpression expr
        resolveVariantKey :: VariantKey -> SomeValue
        resolveVariantKey (VariantKey (Left (Identifier identifier))) = StringValue identifier
        resolveVariantKey (VariantKey (Right number)) = NumberValue $ number ^. numberIso
        resolveExpression :: Expression -> Either String SomeValue
        resolveExpression (Inline expr) = resolveInlineExpression expr
        resolveExpression (Select expr (VariantList variants)) = do
            selector <- resolveInlineExpression expr
            Variant{value} <-
                maybeToEither ("Something not found") $
                    List.find
                        ( \Variant{..} ->
                            Value.matches locales selector (resolveVariantKey key)
                        )
                        variants
                        <|> List.find isDefault variants
            resolvePattern bundle scope value
        formatExpression :: Expression -> Either String Text
        formatExpression = format locales <=< resolveExpression
        resolveLiteral :: Literal -> Either String SomeValue
        resolveLiteral (Left number) = Right . NumberValue $ number ^. numberIso
        resolveLiteral (Right (StringLiteral s)) = Right $ StringValue s
        resolveInlineExpression :: InlineExpression -> Either String SomeValue
        resolveInlineExpression (StringLiteralExpression s) = resolveLiteral $ Right s
        resolveInlineExpression (NumberLiteralExpression n) = resolveLiteral $ Left n
        resolveInlineExpression (FunctionReference (Identifier identifier) CallArguments{..}) = do
            f <- maybeToEither ("Function not found: " <> Text.unpack identifier) $ Map.lookup identifier functions
            positionalArguments' <- mapM resolveInlineExpression positionalArguments
            namedArguments' <- forM namedArguments \(Identifier identifier', literal) -> (identifier',) <$> resolveLiteral literal
            f positionalArguments' $ Map.fromList namedArguments'
        resolveInlineExpression (MessageReference (Identifier identifier) attribute) = do
            message <- maybeToEither ("Message not found: " <> Text.unpack identifier) $ getMessage bundle identifier
            pattern <-
                case attribute of
                    Nothing -> maybeToEither "Message has no value" $ getValue message
                    Just (Identifier attribute) -> do
                        Attribute (Syntax.Attribute _ pattern) <-
                            maybeToEither
                                ("Message attribute not found: " <> Text.unpack (identifier <> "." <> attribute))
                                (getAttribute attribute message)
                        pure pattern
            resolvePattern bundle scope pattern
        resolveInlineExpression (TermReference (Identifier identifier) attribute args) = do
            (pattern, attrs) <- maybeToEither ("Term not found: " <> Text.unpack identifier) $ getTerm bundle identifier
            args <-
                case args of
                    Nothing -> pure mempty
                    Just CallArguments{..}
                        | null positionalArguments ->
                            forM namedArguments \(Identifier identifier', literal) ->
                                (identifier',) <$> resolveLiteral literal
                        | otherwise -> Left "Positional arguments are not allowed"
            pattern <-
                case attribute of
                    Nothing -> pure pattern
                    Just (Identifier attribute) -> do
                        Attribute (Syntax.Attribute _ pattern) <-
                            maybeToEither ("Term attribute not found: " <> Text.unpack (identifier <> "." <> attribute)) $
                                List.find
                                    (\(Attribute (Syntax.Attribute (Identifier identifier) _)) -> identifier == attribute)
                                    attrs
                        pure pattern
            resolvePattern bundle (Map.fromList args) pattern
        resolveInlineExpression (VariableReference (Identifier identifier)) =
            maybeToEither
                ("Identifier " <> Text.unpack identifier <> " not found")
                (Map.lookup identifier scope)
        resolveInlineExpression (PlaceableExpression expr) = resolveExpression expr

instance {-# OVERLAPPING #-} (FormatPattern r) => FormatPattern (FluentArg Int -> r) where
    formatPattern' :: Bundle -> Map Text SomeValue -> Pattern -> (Text, Int) -> r
    formatPattern' b args p (k, v) = formatPattern' b args p (k, NumberValue n)
      where
        n =
            Number
                { numberType = Cardinal
                , numberValue = fromIntegral v
                , minimumFractionDigits = 0
                }

instance {-# OVERLAPPING #-} (FormatPattern r) => FormatPattern (FluentArg Pico -> r) where
    formatPattern' :: Bundle -> Map Text SomeValue -> Pattern -> (Text, Pico) -> r
    formatPattern' b args p (k, v) = formatPattern' b args p (k, NumberValue n)
      where
        n =
            Number
                { numberType = Cardinal
                , numberValue = v
                , minimumFractionDigits = 0
                }

instance {-# OVERLAPPING #-} (FormatPattern r) => FormatPattern (FluentArg Text -> r) where
    formatPattern' :: Bundle -> Map Text SomeValue -> Pattern -> (Text, Text) -> r
    formatPattern' b args p (k, v) = formatPattern' b args p (k, StringValue v)

instance {-# OVERLAPPING #-} (FormatPattern r) => FormatPattern (FluentArg SomeValue -> r) where
    formatPattern' :: Bundle -> Map Text SomeValue -> Pattern -> (Text, SomeValue) -> r
    formatPattern' b args p (k, v) = formatPattern' b (Map.insert k v args) p

instance {-# OVERLAPPING #-} (FormatPattern r) => FormatPattern (Map Text SomeValue -> r) where
    formatPattern' :: Bundle -> Map Text SomeValue -> Pattern -> Map Text SomeValue -> r
    formatPattern' b args p scope = formatPattern' b (scope `Map.union` args) p

instance (Value a, FormatPattern r) => FormatPattern (FluentArg a -> r) where
    formatPattern' :: Bundle -> Map Text SomeValue -> Pattern -> (Text, a) -> r
    formatPattern' b args p (k, v) = formatPattern' b args p (k, SomeValue $ CustomValue v)

formatPattern :: (FormatPattern r) => Bundle -> Pattern -> r
formatPattern b p = formatPattern' b mempty p
