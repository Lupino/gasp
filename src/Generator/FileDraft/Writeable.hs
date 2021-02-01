module Generator.FileDraft.Writeable
       ( Writeable(..)
       ) where

import           Generator.Common                   (ProjectRootDir)
import           Generator.FileDraft.WriteableMonad
import           Generator.Templates                (DataDir)
import           StrongPath                         (Abs, Dir, Path)


class Writeable w where
    -- | Write file somewhere in the provided project root directory.
    write :: (WriteableMonad m)
          => Path Abs (Dir ProjectRootDir)
          -> Path Abs (Dir DataDir)
          -> w
          -> m ()
