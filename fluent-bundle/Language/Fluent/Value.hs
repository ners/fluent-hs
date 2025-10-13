module Language.Fluent.Value where

import Control.Lens (Iso', from, iso, (^.))
import Data.Fixed (Pico, showFixed)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Fluent.Locale (Locale (Locale))
import Language.Fluent.Syntax.Literal (NumberLiteral (NumberLiteral), _NumberLiteral)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Text.Read (Read (readPrec), readMaybe)
import Prelude

data NumberType = Cardinal | Ordinal
    deriving stock (Bounded, Enum, Show, Eq)

class Value a where
    format :: [Locale] -> a -> Either String Text

data Number = Number
    { numberType :: NumberType
    , numberValue :: Pico
    , minimumFractionDigits :: Int
    }
    deriving stock (Eq, Show)

numberIso :: Iso' NumberLiteral Number
numberIso = iso from to
  where
    readPico :: Text -> Pico
    readPico = read . Text.unpack
    fractionDigits :: Text -> Int
    fractionDigits t =
        case Text.split (== '.') t of
            [_] -> 0
            [_, frac] | frac /= "" -> Text.length frac
            _ -> error $ "fractionDigits: could not parse number: " <> Text.unpack t
    from :: NumberLiteral -> Number
    from (NumberLiteral t) =
        Number
            { numberType = Cardinal
            , numberValue = readPico t
            , minimumFractionDigits = fractionDigits t
            }
    to :: Number -> NumberLiteral
    to Number{..} =
        let
            t = Text.pack . showFixed True $ numberValue
            d = fractionDigits t
            hasFrac = d > 0
         in
            NumberLiteral $
                if d < minimumFractionDigits
                    then
                        t <> (if hasFrac then "" else ".") <> (Text.replicate (minimumFractionDigits - d) "0")
                    else
                        t

data SomeValue
    = StringValue Text
    | NumberValue Number
    | SomeValue CustomValue

data CustomValue = forall a. (Value a) => CustomValue a

instance Value SomeValue where
    format locales (StringValue s) = format locales s
    format locales (NumberValue n) = format locales n
    format locales (SomeValue a) = format locales a

instance Value CustomValue where
    format locales (CustomValue a) = format locales a

instance Value Text where format _ = Right . id

instance Value String where format locales = format locales . Text.pack

instance Value Number where
    -- TODO: this should use the locales
    format _ number = Right $ number ^. from numberIso . _NumberLiteral

data PluralCategory
    = Zero
    | One
    | Two
    | Few
    | Many
    | Other
    deriving stock (Bounded, Enum, Eq)

instance Show PluralCategory where
    show Zero = "zero"
    show One = "one"
    show Two = "two"
    show Few = "few"
    show Many = "many"
    show Other = "other"

instance Read PluralCategory where
    readPrec = ReadPrec.lift . ReadP.choice $ [minBound .. maxBound] <&> \a -> a <$ ReadP.string (show a)

matches :: [Locale] -> SomeValue -> SomeValue -> Bool
matches _ SomeValue{} _ = False
matches _ _ SomeValue{} = False
matches _ (StringValue s1) (StringValue s2) = s1 == s2
matches _ (NumberValue n1) (NumberValue n2) = n1 == n2
matches locales (StringValue s) (NumberValue n) = matches locales (NumberValue n) (StringValue s)
matches locales (NumberValue n) (StringValue s)
    | Just cat <- readMaybe @PluralCategory (Text.unpack s) = pluralCategory locales n == cat
    | otherwise = False

pluralCategory :: [Locale] -> Number -> PluralCategory
pluralCategory [] _ = error "pluralCategory: no supported locale"
pluralCategory (Locale "en" : _) Number{..} =
    case numberType of
        Cardinal | minimumFractionDigits /= 0 -> Other
        Cardinal | numberInt == 1 -> One
        Cardinal -> Other
        Ordinal | minimumFractionDigits /= 0 -> Other
        Ordinal | numberInt `mod` 10 == 1 && numberInt `mod` 100 /= 11 -> One
        Ordinal | numberInt `mod` 10 == 2 && numberInt `mod` 100 /= 12 -> Two
        Ordinal | numberInt `mod` 10 == 3 && numberInt `mod` 100 /= 13 -> Few
        Ordinal -> Other
  where
    numberInt :: Int
    numberInt = round numberValue
pluralCategory (_ : locales) n = pluralCategory locales n
