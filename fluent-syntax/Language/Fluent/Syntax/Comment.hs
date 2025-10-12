module Language.Fluent.Syntax.Comment where

import Control.Lens (Cons (_Cons), iso)
import Control.Lens.TH (makePrisms)
import Control.SIArrow (SIArrow (simany), (/$/), (/*/))
import Data.Syntax (Syntax (satisfy))
import Data.Syntax.Char (SyntaxChar, SyntaxText)
import Data.Text (Text)
import Util
import Prelude

newtype Comment = Comment Text
    deriving stock (Show)

makePrisms ''Comment

comment :: (SyntaxChar syn) => syn () Comment
comment =
    (_Comment . textIso . _Cons)
        /$/ satisfy (== '#')
        /*/ simany (satisfy (/= '\n'))
