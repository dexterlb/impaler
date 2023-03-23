module PrimitiveData
    ( Identifier(..)
    )

where

import Data.Text (Text)
import Data.String (IsString)

newtype Identifier = Identifier Text
    deriving newtype (Show, Eq, Ord, IsString)
