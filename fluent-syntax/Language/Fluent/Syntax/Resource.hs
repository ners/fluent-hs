module Language.Fluent.Syntax.Resource where

import Control.Lens (iso)
import Control.SIArrow (SIArrow (sisome), (/$/))
import Data.Either (rights)
import Data.Syntax.Char (SyntaxChar)
import Data.Text (Text)
import Language.Fluent.Syntax.Entry
import Util (blankBlock)
import Util qualified
import Prelude

newtype Resource = Resource {entries :: [Entry]}
    deriving stock (Eq, Show)

resource :: (SyntaxChar syn) => syn () Resource
resource = iso fromResource toResource /$/ sisome (Util.either blankBlock entry)
  where
    toResource :: [Either () Entry] -> Resource
    toResource = Resource . rights

    fromResource :: Resource -> [Either () Entry]
    fromResource (Resource entries) = Right <$> entries

parse :: Text -> Either String Resource
parse = Util.parse resource

print :: Resource -> Either String Text
print = Util.print resource
