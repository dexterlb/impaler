module Utils.Files
    ( browseCode
    )
where

import Path.IO (walkDirAccumRel)
import Path.Internal.Posix (Path)
import Path.Posix (Dir)

browseCode :: Path b Dir -> IO [(Path _Abs _File, Path _Rel _File)]
browseCode dir = walkDirAccumRel Nothing addFiles dir
    where
        addFiles :: _ -> [_] -> [_] -> IO [_]
        addFiles _ _ fpaths = fpaths
