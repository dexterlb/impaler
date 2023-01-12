module DebugInfo
    ( DebugInfo(..)
    , LocationInfo(..)
    , noDebugInfo
    , debugOffset
    )
where

import Data.Text (Text)

data DebugInfo = DebugInfo
    { location :: Maybe LocationInfo
    }
    deriving stock (Show)

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
    deriving stock (Show)
