module Generator.FileDraft.Writeable
  ( Writeable(..)
  ) where

import           Generator.FileDraft.WriteableMonad
import           Path                               (Abs, Dir, Path)


class Writeable w where
    -- | Write file somewhere in the provided project root directory.
    write :: (WriteableMonad m)
          => Path Abs Dir
          -> w
          -> m ()
