module Language.Fluent.Expression where

import Control.Category.Structures ((/+/))
import Control.Lens (Iso', iso)
import Control.Lens.TH (makePrisms)
import Control.SIArrow (SIArrow (sisome), (*/), (/$/), (/*), (/*/))
import Data.Either (partitionEithers)
import Data.Syntax (Syntax (char, satisfy))
import Data.Syntax.Char (SyntaxText, spaces, spaces_)
import Data.Syntax.Combinator (sepBy1)
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Fluent.Attribute
import Language.Fluent.Identifier
import Prelude
import Language.Fluent.Literal

data Expression
    = Select InlineExpression [Variant]
    | Inline InlineExpression

data InlineExpression
    = StringLiteralExpression StringLiteral
    | NumberLiteralExpression NumberLiteral
    | FunctionReference Identifier CallArguments
    | MessageReference Identifier (Maybe Identifier)
    | TermReference Identifier (Maybe Attribute) (Maybe CallArguments)
    | VariableReference Identifier
    | PlaceableExpression Expression

data Variant = Variant
    { key :: VariantKey
    , value :: Pattern
    , isDefault :: Bool
    }

newtype VariantKey = VariantKey (Either Identifier NumberLiteral)

data PatternElement
    = TextElement Text
    | PlaceablePattern Expression

newtype Pattern = Pattern [PatternElement]

data CallArguments = CallArguments
    { positionalArguments :: [InlineExpression]
    , namedArguments :: [(Identifier, InlineExpression)]
    }

makePrisms ''PatternElement

makePrisms ''Pattern

makePrisms ''Expression

makePrisms ''InlineExpression

makePrisms ''Variant

makePrisms ''VariantKey

makePrisms ''CallArguments

$(makePrisms ''Either)

expression :: (SyntaxText syn) => syn () Expression
expression = undefined

inlineExpression :: (SyntaxText syn) => syn () InlineExpression
inlineExpression =
    foldr1
        (/+/)
        [ (_StringLiteralExpression . _StringLiteral . iso Text.unpack Text.pack) /$/ sisome (satisfy (== 'a'))
        , _NumberLiteralExpression /$/ numberLiteral
        , _FunctionReference /$/ identifier /*/ callArguments
        ]

either' :: (SyntaxText syn) => syn () a -> syn () b -> syn () (Either a b)
either' a b = _Left /$/ a /+/ _Right /$/ b

callArguments :: (SyntaxText syn) => syn () CallArguments
callArguments = foo /$/ char '(' */ spaces_ */ sepBy1 (either' namedArgument inlineExpression) (spaces_ */ char ',' */ spaces) /* spaces_ /* char ')'
  where
    foo :: Iso' CallArguments [Either (Identifier, InlineExpression) InlineExpression]
    foo =
        iso
            (\CallArguments{..} -> (Left <$> namedArguments) <> (Right <$> positionalArguments))
            \args ->
                let (namedArguments, positionalArguments) = partitionEithers args
                 in CallArguments{..}

namedArgument :: (SyntaxText syn) => syn () (Identifier, InlineExpression)
namedArgument = undefined
