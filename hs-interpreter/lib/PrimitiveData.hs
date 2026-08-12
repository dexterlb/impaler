module PrimitiveData
  ( Identifier (..),
  )
where

import Data.String (IsString)
import Data.Text (Text)

newtype Identifier = Identifier Text
  deriving newtype (Show, Eq, Ord, IsString)
