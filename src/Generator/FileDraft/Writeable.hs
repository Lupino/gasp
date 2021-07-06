module Generator.FileDraft.Writeable
  ( Writeable(..)
  ) where

import           Generator.FileDraft.WriteableMonad


class Writeable w where
    -- | Write file somewhere in the provided project root directory.
    write :: (WriteableMonad m)
          => FilePath
          -> w
          -> m ()
