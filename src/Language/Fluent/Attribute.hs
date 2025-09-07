module Language.Fluent.Attribute where

import Control.Lens.TH (makePrisms)

data Attribute

makePrisms ''Attribute
