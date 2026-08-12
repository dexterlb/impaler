module Utils.Files
  ( browseCode,
    resolveDir,
    resolveDir',
  )
where

import Control.Monad.IO.Class (MonadIO)
import Path.IO (WalkAction (..), resolveDir, resolveDir', walkDirAccumRel)
import Path.Posix (Abs, Dir, File, Path, Rel, (</>))

browseCode :: forall io. (MonadIO io) => Path Abs Dir -> io [(Path Rel File, Path Abs File)]
browseCode dir = walkDirAccumRel (Just descend) addFiles dir
  where
    descend :: Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> io (WalkAction Rel)
    descend _ _ _ = pure $ WalkExclude []

    addFiles :: Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> io [(Path Rel File, Path Abs File)]
    addFiles ldir _ fpaths = pure $ map ((absAndRel dir) . (ldir </>)) fpaths

absAndRel :: Path Abs Dir -> Path Rel File -> (Path Rel File, Path Abs File)
absAndRel dir relf = (relf, dir </> relf)
