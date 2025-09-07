module Language.Fluent.Comment where

import Control.Lens (Cons (_Cons), iso)
import Control.Lens.TH (makePrisms)
import Control.SIArrow (SIArrow (simany), (/$/), (/*/))
import Data.Syntax (Syntax (satisfy))
import Data.Syntax.Char (SyntaxText)
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude

newtype Comment = Comment Text

makePrisms ''Comment

comment :: (SyntaxText syn) => syn () Comment
comment =
    (_Comment . iso Text.unpack Text.pack . _Cons)
        /$/ satisfy (== '#')
        /*/ simany (satisfy (/= '\n'))
