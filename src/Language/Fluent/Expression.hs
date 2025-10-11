module Language.Fluent.Expression where

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
import Data.Syntax.Combinator (choice, opt, opt_, optional, sepBy1)
import Data.Text (Text)
import Debug.Trace (trace)
import Language.Fluent.Identifier
import Language.Fluent.Literal
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
    | TermReference Identifier (Maybe Attribute) (Maybe CallArguments)
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
    , namedArguments :: [(Identifier, InlineExpression)]
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
expression = trace "expression" $ iso fromExpression toExpression /$/ inlineExpression /*/ optional (char 'x' */ blank */ char '-' */ char '>' */ blankInline */ variantList)
  where
    fromExpression (Inline i) = (i, Nothing)
    fromExpression (Select i v) = (i, Just v)
    toExpression (i, v) = trace "toExpression" $ maybe (Inline i) (Select i) v

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
    inlinePlaceable :: (SyntaxChar syn) => syn () Expression
    inlinePlaceable = trace "inlinePlaceable" $ char '{' */ opt_ blank */ expression /* opt_ blank /* char '}'
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

textChar :: (SyntaxChar syn) => syn () Char
textChar = satisfy (`notElem` ("{}\r\n" :: String))

quotedChar :: (SyntaxChar syn) => syn () Char
quotedChar = satisfy (`notElem` ("\"\\\r\n" :: String))

variantList :: (SyntaxChar syn) => syn () VariantList
variantList = _VariantList . semiIso validate validate /$/ simany variant
  where
    validate :: [Variant] -> Either String [Variant]
    validate vs =
        case filter isDefault vs of
            [_] -> Right vs
            [] -> Left "no default variant"
            _ -> Left "more than one default variant"

variant :: (SyntaxChar syn) => syn () Variant
variant = iso fromVariant toVariant /$~ (endOfLine */ blank */ optional (char '*') /*/ variantKey /*/ (blankInline */ pattern_))
  where
    fromVariant Variant{..} = (if isDefault then Just () else Nothing, key, value)
    toVariant (isJust @() -> isDefault, key, value) = Variant{..}

variantKey :: (SyntaxChar syn) => syn () VariantKey
variantKey = undefined

inlineExpression :: (SyntaxChar syn) => syn () InlineExpression
inlineExpression =
    trace "inlineExpression" $
        choice
            [ _StringLiteralExpression . _StringLiteral . textIso /$/ char '"' */ sisome quotedChar /* char '"'
            , _NumberLiteralExpression /$/ numberLiteral
            , _FunctionReference /$/ identifier /*/ callArguments
            , _VariableReference /$/ char '$' */ identifier
            ]

callArguments :: (SyntaxChar syn) => syn () CallArguments
callArguments = foo /$/ char '(' */ blankInline */ sepBy1 (Util.either namedArgument inlineExpression) (blankInline */ char ',' */ blankInline) /* blankInline /* char ')'
  where
    foo :: Iso' CallArguments [Either (Identifier, InlineExpression) InlineExpression]
    foo =
        iso
            (\CallArguments{..} -> (Left <$> namedArguments) <> (Right <$> positionalArguments))
            \args ->
                let (namedArguments, positionalArguments) = partitionEithers args
                 in CallArguments{..}

namedArgument :: (SyntaxChar syn) => syn () (Identifier, InlineExpression)
namedArgument = undefined
