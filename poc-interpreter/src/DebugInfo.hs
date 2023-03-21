module DebugInfo
    ( DebugInfo(..)
    , LocationInfo(..)
    , noDebugInfo
    , debugOffset
    , builtinDebugInfo
    )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

data DebugInfo = DebugInfo
    { location :: Maybe LocationInfo
    }

noDebugInfo :: DebugInfo
noDebugInfo = DebugInfo
    { location = Nothing
    }

debugOffset :: Int -> Int -> DebugInfo
debugOffset offBefore offAfter = DebugInfo
    { location = Just $ LocationInfo
        { offsetBefore = offBefore
        , offsetAfter = offAfter
        , fileName = ""
        }
    }

data LocationInfo = LocationInfo
    { offsetBefore :: Int
    , offsetAfter :: Int
    , fileName :: Text
    }

builtinDebugInfo :: DebugInfo
builtinDebugInfo = DebugInfo
    { location = Just $ LocationInfo
        { offsetBefore = 0
        , offsetAfter = 0
        , fileName = "<builtin>"
        }
    }

instance (Show DebugInfo) where
    show d = "{" <> (fromMaybe "" $ show <$> d.location) <> "}"

instance (Show LocationInfo) where
    show l = (T.unpack l.fileName) <> ":" <> (show l.offsetBefore) <> ":" <> (show l.offsetAfter)
