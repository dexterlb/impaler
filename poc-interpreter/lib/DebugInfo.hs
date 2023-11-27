module DebugInfo
  ( DebugInfo (..),
    LocationInfo (..),
    noDebugInfo,
    debugOffset,
    builtinDebugInfo,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Utils.Parsing qualified as P

data DebugInfo = DebugInfo
  { location :: Maybe LocationInfo
  }

noDebugInfo :: DebugInfo
noDebugInfo =
  DebugInfo
    { location = Nothing
    }

debugOffset :: P.SourcePos -> P.SourcePos -> DebugInfo
debugOffset offBefore offAfter =
  DebugInfo
    { location =
        Just $
          LocationInfo
            { offsetBefore = offBefore,
              offsetAfter = offAfter
            }
    }

data LocationInfo = LocationInfo
  { offsetBefore :: P.SourcePos,
    offsetAfter :: P.SourcePos
  }

builtinDebugInfo :: DebugInfo
builtinDebugInfo =
  DebugInfo
    { location =
        Just $
          LocationInfo
            { offsetBefore = P.initialPos fn,
              offsetAfter = P.initialPos fn
            }
    }
  where
    fn = "<builtin>"

instance (Show DebugInfo) where
  show d = "{" <> (fromMaybe "" $ show <$> d.location) <> "}"

instance (Show LocationInfo) where
  show = T.unpack . stringifyLocationInfo

stringifyLocationInfo :: LocationInfo -> Text
stringifyLocationInfo
  ( LocationInfo
      { offsetBefore = (P.SourcePos {sourceName = ssn1, sourceLine = pl1, sourceColumn = pc1}),
        offsetAfter = (P.SourcePos {sourceName = ssn2, sourceLine = pl2, sourceColumn = pc2})
      }
    )
    | sn1 == sn2 && l1 == l2 = left <> c2
    | sn1 == sn2 = left <> l2 <> ":" <> c2
    | otherwise = left <> sn2 <> ":" <> l2 <> ":" <> c2
    where
      left = sn1 <> ":" <> l1 <> ":" <> c1 <> "-"
      sn1 = T.pack $ ssn1
      sn2 = T.pack $ ssn2
      l1 = T.pack $ show $ P.unPos pl1
      l2 = T.pack $ show $ P.unPos pl2
      c1 = T.pack $ show $ P.unPos pc1
      c2 = T.pack $ show $ P.unPos pc2
