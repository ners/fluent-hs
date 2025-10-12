module Language.Fluent.Syntax.Expression where

import Control.Arrow ((>>>))
import Control.Category.Structures ((/+/))
import Control.Lens (Iso', iso, _Cons)
import Control.Lens.SemiIso (constant, semiIso)
import Control.Lens.TH (makePrisms)
import Control.SIArrow (SIArrow (sibind, simany, sisome), (#>>), (*/), (/$/), (/$~), (/*), (/*/))
import Data.Bool (bool)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe, isJust)
import Data.Syntax (Syntax (char, satisfy))
import Data.Syntax.Char (SyntaxChar, endOfLine)
import Data.Syntax.Combinator (choice, opt, opt_, optional, sepBy, sepBy1)
import Data.Text (Text)
import Debug.Trace (trace)
import Language.Fluent.Syntax.Identifier
import Language.Fluent.Syntax.Literal
import Util
import Prelude

data Attribute = Attribute Identifier Pattern
    deriving stock (Show)

data Expression
    = Select InlineExpression VariantList
    | Inline InlineExpression
    deriving stock (Show)

data InlineExpression
    = StringLiteralExpression StringLiteral
    | NumberLiteralExpression NumberLiteral
    | FunctionReference Identifier CallArguments
    | MessageReference Identifier (Maybe Identifier)
    | TermReference Identifier (Maybe Identifier) (Maybe CallArguments)
    | VariableReference Identifier
    | PlaceableExpression Expression
    deriving stock (Show)

data Variant = Variant
    { key :: VariantKey
    , value :: Pattern
    , isDefault :: Bool
    }
    deriving stock (Show)

newtype VariantKey = VariantKey (Either Identifier NumberLiteral)
    deriving stock (Show)

newtype VariantList = VariantList [Variant]
    deriving stock (Show)

data PatternElement
    = TextElement Text
    | PlaceablePattern Expression
    deriving stock (Show)

newtype Pattern = Pattern [PatternElement]
    deriving stock (Show)

data CallArguments = CallArguments
    { positionalArguments :: [InlineExpression]
    , namedArguments :: [(Identifier, Literal)]
    }
    deriving stock (Show)

makePrisms ''PatternElement

makePrisms ''Pattern

makePrisms ''Expression

makePrisms ''InlineExpression

makePrisms ''Variant

makePrisms ''VariantKey

makePrisms ''VariantList

makePrisms ''CallArguments

makePrisms ''Attribute

attribute :: (SyntaxChar syn) => syn () Attribute
attribute = _Attribute /$~ endOfLine /*/ opt blank /*/ char '.' /*/ identifier /*/ opt blank /*/ char '=' /*/ opt blank /*/ pattern_

expression :: (SyntaxChar syn) => syn () Expression
expression = trace "expression" $ iso fromExpression toExpression /$/ inlineExpression /*/ optional selectSubexpression
  where
    selectSubexpression :: (SyntaxChar syn) => syn () VariantList
    selectSubexpression = trace "selectExpression" $ opt blank */ char '-' */ char '>' */ opt blankInline */ variantList

    fromExpression (Inline i) = (i, Nothing)
    fromExpression (Select i v) = (i, Just v)
    toExpression (i, v) = maybe (trace "toExpression Inline" $ Inline i) (trace "toExpression Seelect" $ Select i) v

pattern_ :: (SyntaxChar syn) => syn () Pattern
pattern_ = _Pattern /$/ sisome patternElement

patternElement :: (SyntaxChar syn) => syn () PatternElement
patternElement =
    choice
        [ blankBlock */ block
        , _TextElement . textIso /$/ inlineText
        , _PlaceablePattern /$/ inlinePlaceable
        ]
  where
    inlineText :: (SyntaxChar syn) => syn () String
    inlineText = sisome textChar
    indentedChar :: (SyntaxChar syn) => syn () Char
    indentedChar = satisfy (`notElem` ("{}[*.\r\n" :: String))
    block :: forall syn. (SyntaxChar syn) => syn () PatternElement
    block = Util.bool blankInline >>> sibind (iso f g)
      where
        placeablePattern, textBlock :: syn () PatternElement
        placeablePattern = _PlaceablePattern /$/ inlinePlaceable
        textBlock = _TextElement . textIso . _Cons /$/ indentedChar /*/ (iso Just (fromMaybe mempty) /$/ optional inlineText)
        f :: Bool -> syn Bool PatternElement
        f b = constant b #>> Data.Bool.bool placeablePattern (placeablePattern /+/ textBlock) b
        g :: PatternElement -> syn Bool PatternElement
        g = const $ f True

inlinePlaceable :: (SyntaxChar syn) => syn () Expression
inlinePlaceable = trace "inlinePlaceable" $ char '{' */ opt_ blank */ expression /* opt_ blank /* char '}'

textChar :: (SyntaxChar syn) => syn () Char
textChar = satisfy (`notElem` ("{}\r\n" :: String))

variantList :: (SyntaxChar syn) => syn () VariantList
variantList = trace "variantList" $ _VariantList . semiIso validate validate /$/ simany variant
  where
    validate :: [Variant] -> Either String [Variant]
    validate vs =
        case filter isDefault vs of
            [_] -> Right vs
            [] -> Left "no default variant"
            _ -> Left "more than one default variant"

variant :: (SyntaxChar syn) => syn () Variant
variant = iso fromVariant toVariant /$~ endOfLine */ opt_ blank */ optional (char '*') /*/ variantKey /*/ (opt_ blankInline */ pattern_)
  where
    fromVariant Variant{..} = (if isDefault then Just () else Nothing, key, value)
    toVariant (isJust @() -> isDefault, key, value) = Variant{..}

variantKey :: (SyntaxChar syn) => syn () VariantKey
variantKey = _VariantKey /$/ char '[' */ opt_ blank */ Util.either identifier numberLiteral /* opt_ blank /* char ']'

inlineExpression :: (SyntaxChar syn) => syn () InlineExpression
inlineExpression =
    trace "inlineExpression" $
        choice
            [ _StringLiteralExpression /$/ stringLiteral
            , _NumberLiteralExpression /$/ numberLiteral
            , trace "function reference" $ _FunctionReference /$/ identifier /*/ callArguments
            , _MessageReference /$/ identifier /*/ optional attributeAccessor
            , _TermReference /$~ char '-' */ identifier /*/ optional attributeAccessor /*/ optional callArguments
            , _VariableReference /$/ char '$' */ identifier
            , _PlaceableExpression /$/ inlinePlaceable
            ]
  where
    attributeAccessor :: (SyntaxChar syn) => syn () Identifier
    attributeAccessor = char '.' */ identifier

callArguments :: (SyntaxChar syn) => syn () CallArguments
callArguments = foo /$/ opt_ blank */ char '(' */ opt_ blank */ argumentList /* opt blank /* char ')'
  where
    argumentList :: (SyntaxChar syn) => syn () [Either (Identifier, Literal) InlineExpression]
    argumentList = sepBy (Util.either namedArgument inlineExpression) (opt_ blank */ char ',' */ opt blank)

    foo :: Iso' CallArguments [Either (Identifier, Literal) InlineExpression]
    foo =
        iso
            (\CallArguments{..} -> (Left <$> namedArguments) <> (Right <$> positionalArguments))
            \args ->
                let (namedArguments, positionalArguments) = partitionEithers args
                 in CallArguments{..}

namedArgument :: (SyntaxChar syn) => syn () (Identifier, Literal)
namedArgument = (identifier /* opt_ blank /* char ':' /* opt blank) /*/ literal
