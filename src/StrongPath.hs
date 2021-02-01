{-# LANGUAGE PartialTypeSignatures #-}
module StrongPath
    ( Path, Path'
    , Abs, Rel, Dir, File, File'
    , System, Windows, Posix

    , parseRelDir, parseRelFile, parseAbsDir, parseAbsFile
    , parseRelDirW, parseRelFileW, parseAbsDirW, parseAbsFileW
    , parseRelDirP, parseRelFileP, parseAbsDirP, parseAbsFileP

    , fromPathRelDir, fromPathRelFile, fromPathAbsDir, fromPathAbsFile
    , fromPathRelDirW, fromPathRelFileW, fromPathAbsDirW, fromPathAbsFileW
    , fromPathRelDirP, fromPathRelFileP, fromPathAbsDirP, fromPathAbsFileP

    , toPathRelDir, toPathRelFile, toPathAbsDir, toPathAbsFile
    , toPathRelDirW, toPathRelFileW, toPathAbsDirW, toPathAbsFileW
    , toPathRelDirP, toPathRelFileP, toPathAbsDirP, toPathAbsFileP

    , fromRelDir, fromRelFile, fromAbsDir, fromAbsFile
    , fromRelDirP, fromRelFileP, fromAbsDirP, fromAbsFileP
    , fromRelDirW, fromRelFileW, fromAbsDirW, fromAbsFileW

    , toFilePath

    , (</>)

    , castRel, castDir

    , parent

    , relDirToPosix, relFileToPosix, relDirToPosix', relFileToPosix'
    ) where

import           Control.Monad.Catch     (MonadThrow)
import           Data.List               (intercalate)
import           Data.Maybe              (fromJust)
import qualified Path                    as P
import qualified Path.Posix              as PP
import qualified Path.Windows            as PW
import qualified System.FilePath         as FP
import qualified System.FilePath.Posix   as FPP
import qualified System.FilePath.Windows as FPW

import           StrongPath.Internal


-- TODO: We still depend on Path for creating hardcoded paths via generics. Any way to go around that?
--   Maybe implement our own mechanism for that, so that people don't have to know about / use Path?
--   This means we would implement our own [reldir|foobar|] stuff.

-- TODO: Can I use type classes and return type polymorhipsm to make all this shorter and reduce duplication?
-- class Path, and then I have PathWindows and PathPosix and PathSystem implement it, smth like that?
-- And then fromPathRelDir has polymorhic return type based on standard? I tried a little bit but it is complicated.

-- TODO: If there is no other solution to all this duplication, do some template haskell magic to simplify it.

-- TODO: Redo the types naming? Normal types should be Path Rel Dir File, while shortened ones should be
--   Path' Rel' Dir' File'.
--   This means that Path' is alias for Path System, Rel' for Rel (), Dir' for Dir (), File' for File ().

-- Constructors
-- TODO: Although here I specify which exact type of Path (P.Path, PP.Path or PW.Path) is to be
--   given as first argument, I realized that if I do:
--     SP.fromPathRelDirW [P.reldir|test\file|]
--   compiler will not complain, although I put P instead of PW!
--   I am not sure why is this happening, we should figure it out.
--   This is not great because it means somebody can by accident construct
--   StrongPath that should be Windows but is really Posix.
--   Or can they? I am not sure if P.Path is just considered the same as PW.Path,
--   or P.relfile and PW.relfile and PP.relfile for some weird reason are polymorhic
--   in return type, or what is happening. I believe it is something close to the latter,
--   in which case it is less of a problem, but I am not sure.
--   Actually, it also does not complain if I do:
--     SP.fromPathRelFileP [P.reldir|test/file|]
--   so although I put reldir, and it should be relfile, it does not complain! How is that possible!?
--   If I put absdir, then it does complain, however not if I put reldir. Very weird.
--   NOTE: In Path, Path.Windows.Path and Path.Posix.Path are actually the same Path it seems
--     so compiler does not differentiate them (because they are all exporting the same module containing Path),
--     but Path.Windows.Rel and Path.Posix.Rel (and same for Abs/Dir/File) are not the same,
--     because they are done via Include mechanism.
fromPathRelDir   :: P.Path  P.Rel  P.Dir   -> Path' System  (Rel a) (Dir b)
fromPathRelFile  :: P.Path  P.Rel  P.File  -> Path' System  (Rel a) (File' f)
fromPathAbsDir   :: P.Path  P.Abs  P.Dir   -> Path' System  Abs     (Dir a)
fromPathAbsFile  :: P.Path  P.Abs  P.File  -> Path' System  Abs     (File' f)
fromPathRelDirW  :: PW.Path PW.Rel PW.Dir  -> Path' Windows (Rel a) (Dir b)
fromPathRelFileW :: PW.Path PW.Rel PW.File -> Path' Windows (Rel a) (File' f)
fromPathAbsDirW  :: PW.Path PW.Abs PW.Dir  -> Path' Windows Abs     (Dir a)
fromPathAbsFileW :: PW.Path PW.Abs PW.File -> Path' Windows Abs     (File' f)
fromPathRelDirP  :: PP.Path PP.Rel PP.Dir  -> Path' Posix   (Rel a) (Dir b)
fromPathRelFileP :: PP.Path PP.Rel PP.File -> Path' Posix   (Rel a) (File' f)
fromPathAbsDirP  :: PP.Path PP.Abs PP.Dir  -> Path' Posix   Abs     (Dir a)
fromPathAbsFileP :: PP.Path PP.Abs PP.File -> Path' Posix   Abs     (File' f)
---- System
fromPathRelDir p  = RelDir p NoPrefix
fromPathRelFile p  = RelFile p NoPrefix
fromPathAbsDir   = AbsDir
fromPathAbsFile  = AbsFile
---- Windows
fromPathRelDirW p = RelDirW p NoPrefix
fromPathRelFileW p = RelFileW p NoPrefix
fromPathAbsDirW  = AbsDirW
fromPathAbsFileW = AbsFileW
---- Posix
fromPathRelDirP p = RelDirP p NoPrefix
fromPathRelFileP p = RelFileP p NoPrefix
fromPathAbsDirP  = AbsDirP
fromPathAbsFileP = AbsFileP

-- TODO: Should I go with MonadThrow here instead of just throwing error? Probably!
--       I could, as error, return actual Path + info on how many ../ were there in StrongPath,
--       so user can recover from error and continue, if they wish.
-- Deconstructors
toPathRelDir   :: Path' System  (Rel a) (Dir b)   -> P.Path  P.Rel  P.Dir
toPathRelFile  :: Path' System  (Rel a) (File' f) -> P.Path  P.Rel  P.File
toPathAbsDir   :: Path' System  Abs     (Dir a)   -> P.Path  P.Abs  P.Dir
toPathAbsFile  :: Path' System  Abs     (File' f) -> P.Path  P.Abs  P.File
toPathRelDirW  :: Path' Windows (Rel a) (Dir b)   -> PW.Path PW.Rel PW.Dir
toPathRelFileW :: Path' Windows (Rel a) (File' f) -> PW.Path PW.Rel PW.File
toPathAbsDirW  :: Path' Windows Abs     (Dir a)   -> PW.Path PW.Abs PW.Dir
toPathAbsFileW :: Path' Windows Abs     (File' f) -> PW.Path PW.Abs PW.File
toPathRelDirP  :: Path' Posix   (Rel a) (Dir b)   -> PP.Path PP.Rel PP.Dir
toPathRelFileP :: Path' Posix   (Rel a) (File' f) -> PP.Path PP.Rel PP.File
toPathAbsDirP  :: Path' Posix   Abs     (Dir a)   -> PP.Path PP.Abs PP.Dir
toPathAbsFileP :: Path' Posix   Abs     (File' f) -> PP.Path PP.Abs PP.File
---- System
toPathRelDir (RelDir p NoPrefix) = p
toPathRelDir (RelDir _ _)        = relativeStrongPathWithPrefixToPathError
toPathRelDir _                   = impossible
toPathRelFile (RelFile p NoPrefix) = p
toPathRelFile (RelFile _ _)        = relativeStrongPathWithPrefixToPathError
toPathRelFile _                    = impossible
toPathAbsDir (AbsDir p) = p
toPathAbsDir _          = impossible
toPathAbsFile (AbsFile p) = p
toPathAbsFile _           = impossible
---- Windows
toPathRelDirW (RelDirW p NoPrefix) = p
toPathRelDirW (RelDirW _ _)        = relativeStrongPathWithPrefixToPathError
toPathRelDirW _                    = impossible
toPathRelFileW (RelFileW p NoPrefix) = p
toPathRelFileW (RelFileW _ _)        = relativeStrongPathWithPrefixToPathError
toPathRelFileW _                     = impossible
toPathAbsDirW (AbsDirW p) = p
toPathAbsDirW _           = impossible
toPathAbsFileW (AbsFileW p) = p
toPathAbsFileW _            = impossible
---- Posix
toPathRelDirP (RelDirP p NoPrefix) = p
toPathRelDirP (RelDirP _ _)        = relativeStrongPathWithPrefixToPathError
toPathRelDirP _                    = impossible
toPathRelFileP (RelFileP p NoPrefix) = p
toPathRelFileP (RelFileP _ _)        = relativeStrongPathWithPrefixToPathError
toPathRelFileP _                     = impossible
toPathAbsDirP (AbsDirP p) = p
toPathAbsDirP _           = impossible
toPathAbsFileP (AbsFileP p) = p
toPathAbsFileP _            = impossible

relativeStrongPathWithPrefixToPathError :: a
relativeStrongPathWithPrefixToPathError =
    error "Relative StrongPath.Path with prefix can't be converted into Path.Path."

-- | Parsers.
-- How parsers work:
--        Parsers              From          To
--  parseRel[Dir|File]     System/Posix    System
--  parseRel[Dir|File]W    Win/Posix       Win
--  parseRel[Dir|File]P    Posix           Posix
--  parseAbs[Dir|File]     System/Posix*   System
--  parseAbs[Dir|File]W    Win/Posix*      Win
--  parseAbs[Dir|File]P    Posix           Posix
--
--  NOTE: System/Posix* means that path has to be System with exception of separators
--        that can be Posix besides being System (but e.g. root can't be Posix).
--        Win/Posix* is analogous to System/Posix*.
parseRelDir   :: MonadThrow m => FilePath -> m (Path' System  (Rel d1) (Dir d2))
parseRelFile  :: MonadThrow m => FilePath -> m (Path' System  (Rel d)  (File' f))
parseAbsDir   :: MonadThrow m => FilePath -> m (Path' System  Abs      (Dir d))
parseAbsFile  :: MonadThrow m => FilePath -> m (Path' System  Abs      (File' f))
parseRelDirW  :: MonadThrow m => FilePath -> m (Path' Windows (Rel d1) (Dir d2))
parseRelFileW :: MonadThrow m => FilePath -> m (Path' Windows (Rel d)  (File' f))
parseAbsDirW  :: MonadThrow m => FilePath -> m (Path' Windows Abs      (Dir d))
parseAbsFileW :: MonadThrow m => FilePath -> m (Path' Windows Abs      (File' f))
parseRelDirP  :: MonadThrow m => FilePath -> m (Path' Posix   (Rel d1) (Dir d2))
parseRelFileP :: MonadThrow m => FilePath -> m (Path' Posix   (Rel d)  (File' f))
parseAbsDirP  :: MonadThrow m => FilePath -> m (Path' Posix   Abs      (Dir d))
parseAbsFileP :: MonadThrow m => FilePath -> m (Path' Posix   Abs      (File' f))
---- System
parseRelDir  = parseRelFP RelDir [FP.pathSeparator, FPP.pathSeparator] P.parseRelDir
parseRelFile = parseRelFP RelFile [FP.pathSeparator, FPP.pathSeparator] P.parseRelFile
parseAbsDir fp = fromPathAbsDir <$> P.parseAbsDir fp
parseAbsFile fp = fromPathAbsFile <$> P.parseAbsFile fp
---- Windows
parseRelDirW = parseRelFP RelDirW [FPW.pathSeparator, FPP.pathSeparator] PW.parseRelDir
parseRelFileW = parseRelFP RelFileW [FPW.pathSeparator, FPP.pathSeparator] PW.parseRelFile
parseAbsDirW fp = fromPathAbsDirW <$> PW.parseAbsDir fp
parseAbsFileW fp = fromPathAbsFileW <$> PW.parseAbsFile fp
---- Posix
parseRelDirP = parseRelFP RelDirP [FPP.pathSeparator] PP.parseRelDir
parseRelFileP = parseRelFP RelFileP [FPP.pathSeparator] PP.parseRelFile
parseAbsDirP fp = fromPathAbsDirP <$> PP.parseAbsDir fp
parseAbsFileP fp = fromPathAbsFileP <$> PP.parseAbsFile fp


toFilePath :: Path' s b t -> FilePath
toFilePath sp = case sp of
    ---- System
    RelDir p prefix  -> relPathToFilePath P.toFilePath FP.pathSeparator prefix p
    RelFile p prefix -> relPathToFilePath P.toFilePath FP.pathSeparator prefix p
    AbsDir p         -> P.toFilePath p
    AbsFile p        -> P.toFilePath p
    ---- Windows
    RelDirW p prefix ->  relPathToFilePath PW.toFilePath FPW.pathSeparator prefix p
    RelFileW p prefix ->  relPathToFilePath PW.toFilePath FPW.pathSeparator prefix p
    AbsDirW p ->  PW.toFilePath p
    AbsFileW p ->  PW.toFilePath p
    ---- Posix
    RelDirP p prefix ->  relPathToFilePath PP.toFilePath FPP.pathSeparator prefix p
    RelFileP p prefix ->  relPathToFilePath PP.toFilePath FPP.pathSeparator prefix p
    AbsDirP p ->  PP.toFilePath p
    AbsFileP p ->  PP.toFilePath p
  where
    relPathToFilePath pathToFilePath sep prefix path =
        combinePrefixWithPath sep (relPathPrefixToFilePath sep prefix) (pathToFilePath path)

    relPathPrefixToFilePath :: Char -> RelPathPrefix -> FilePath
    relPathPrefixToFilePath _ NoPrefix = ""
    relPathPrefixToFilePath sep (ParentDir n) =
        intercalate [sep] (replicate n "..") ++ [sep]

    -- TODO: This function and helper functions above are somewhat too loose and hard to
    --   follow, implement them in better way.
    -- Here we are assuming that prefix is of form (../)*, therefore it ends with separator,
    -- and it could also be empty.
    combinePrefixWithPath :: Char -> String -> FilePath -> FilePath
    combinePrefixWithPath sep prefix path
        | path `elem` [".", ['.', sep], "./"] && not (null prefix) = prefix
    combinePrefixWithPath _ prefix path = prefix ++ path

-- These functions just call toFilePath, but their value is in
-- their type: they allow you to capture expected type of the strong path
-- that you want to convert into FilePath.
fromRelDir   :: Path' System  (Rel r) (Dir d)   -> FilePath
fromRelDir   = toFilePath
fromRelFile  :: Path' System  (Rel r) (File' f) -> FilePath
fromRelFile  = toFilePath
fromAbsDir   :: Path' System  Abs     (Dir d)   -> FilePath
fromAbsDir   = toFilePath
fromAbsFile  :: Path' System  Abs     (File' f) -> FilePath
fromAbsFile  = toFilePath
fromRelDirP  :: Path' Posix   (Rel r) (Dir d)   -> FilePath
fromRelDirP  = toFilePath
fromRelFileP :: Path' Posix   (Rel r) (File' f) -> FilePath
fromRelFileP = toFilePath
fromAbsDirP  :: Path' Posix   Abs     (Dir d)   -> FilePath
fromAbsDirP  = toFilePath
fromAbsFileP :: Path' Posix   Abs     (File' f) -> FilePath
fromAbsFileP = toFilePath
fromRelDirW  :: Path' Windows (Rel r) (Dir d)   -> FilePath
fromRelDirW  = toFilePath
fromRelFileW :: Path' Windows (Rel r) (File' f) -> FilePath
fromRelFileW = toFilePath
fromAbsDirW  :: Path' Windows Abs     (Dir d)   -> FilePath
fromAbsDirW  = toFilePath
fromAbsFileW :: Path' Windows Abs     (File' f) -> FilePath
fromAbsFileW = toFilePath

-- | Either removes last entry or if there are no entries and just "../"s, adds one more "../".
--   If path is absolute root and it has no parent, it will return unchanged path, same like Path.
parent :: Path' s b t -> Path' s b (Dir d)
parent path = case path of
    ---- System
    RelDir p prefix   -> relDirPathParent RelDir P.parent p prefix
    RelFile p prefix  -> RelDir (P.parent p) prefix
    AbsDir p          -> AbsDir $ P.parent p
    AbsFile p         -> AbsDir $ P.parent p
    ---- Windows
    RelDirW p prefix  -> relDirPathParent RelDirW PW.parent p prefix
    RelFileW p prefix -> RelDirW (PW.parent p) prefix
    AbsDirW p         -> AbsDirW $ PW.parent p
    AbsFileW p        -> AbsDirW $ PW.parent p
    ---- Posix
    RelDirP p prefix  -> relDirPathParent RelDirP PP.parent p prefix
    RelFileP p prefix ->  RelDirP (PP.parent p) prefix
    AbsDirP p         -> AbsDirP $ PP.parent p
    AbsFileP p        -> AbsDirP $ PP.parent p
  where
    -- NOTE: We need this special logic for RelDir, because if we have RelDir Path,
    --   it is possible that it is "." or smth like that and no parent can be obtained,
    --   in which case we want to add "../" to our prefix.
    --   For file though, we don't have that concern, because it will always be possible to
    --   get a parent, as per current Path implementation.
    relDirPathParent constructor pathParent p prefix =
        if pathParent p == p
        then let prefix' = case prefix of
                     ParentDir n -> ParentDir (n + 1)
                     NoPrefix    -> ParentDir 1
             in constructor p prefix'
        else let p' = pathParent p
             in constructor p' prefix


-- | How "../"s are resolved:
--   For each "../" at the start of the right hand path, one most right entry is removed
--   from the left hand path.
--     Example: "a/b" </> "../c" = "a/c"
--   If left path is absolute and right path has too many "../"s, they go "over" the root
--   and are effectively ignored.
--     Example: "/a/b" </> "../../../c" = "/c"
--   If left path is relative and right path has more "../"s then left has entries,
--   the leftover "../"s are carried over.
--     Example: "a/b" </> "../../../c" = "../c"
(</>) :: Path' s a (Dir d) -> Path' s (Rel d) c -> Path' s a c
---- System
lsp@(RelDir _ _) </> (RelFile rp rprefix) =
    let (RelDir lp' lprefix') = iterate parent lsp !! prefixNumParentDirs rprefix
    in RelFile (lp' P.</> rp) lprefix'
lsp@(RelDir _ _) </> (RelDir rp rprefix) =
    let (RelDir lp' lprefix') = iterate parent lsp !! prefixNumParentDirs rprefix
    in RelDir (lp' P.</> rp) lprefix'
lsp@(AbsDir _) </> (RelFile rp rprefix) =
    let (AbsDir lp') = iterate parent lsp !! prefixNumParentDirs rprefix
    in AbsFile (lp' P.</> rp)
lsp@(AbsDir _) </> (RelDir rp rprefix) =
    let (AbsDir lp') = iterate parent lsp !! prefixNumParentDirs rprefix
    in AbsDir (lp' P.</> rp)
---- Windows
lsp@(RelDirW _ _) </> (RelFileW rp rprefix) =
    let (RelDirW lp' lprefix') = iterate parent lsp !! prefixNumParentDirs rprefix
    in RelFileW (lp' `pathWinCombineRelDirAndRelFile` rp) lprefix'
lsp@(RelDirW _ _) </> (RelDirW rp rprefix) =
    let (RelDirW lp' lprefix') = iterate parent lsp !! prefixNumParentDirs rprefix
    in RelDirW (lp' `pathWinCombineRelDirAndRelDir` rp) lprefix'
lsp@(AbsDirW _) </> (RelFileW rp rprefix) =
    let (AbsDirW lp') = iterate parent lsp !! prefixNumParentDirs rprefix
    in AbsFileW (lp' PW.</> rp)
lsp@(AbsDirW _) </> (RelDirW rp rprefix) =
    let (AbsDirW lp') = iterate parent lsp !! prefixNumParentDirs rprefix
    in AbsDirW (lp' `pathWinCombineAbsDirAndRelDir` rp)
---- Posix
lsp@(RelDirP _ _) </> (RelFileP rp rprefix) =
    let (RelDirP lp' lprefix') = iterate parent lsp !! prefixNumParentDirs rprefix
    in RelFileP (lp' `pathPosixCombineRelDirAndRelFile` rp) lprefix'
lsp@(RelDirP _ _) </> (RelDirP rp rprefix) =
    let (RelDirP lp' lprefix') = iterate parent lsp !! prefixNumParentDirs rprefix
    in RelDirP (lp' `pathPosixCombineRelDirAndRelDir` rp) lprefix'
lsp@(AbsDirP _) </> (RelFileP rp rprefix) =
    let (AbsDirP lp') = iterate parent lsp !! prefixNumParentDirs rprefix
    in AbsFileP (lp' PP.</> rp)
lsp@(AbsDirP _) </> (RelDirP rp rprefix) =
    let (AbsDirP lp') = iterate parent lsp !! prefixNumParentDirs rprefix
    in AbsDirP (lp' `pathPosixCombineAbsDirAndRelDir` rp)
_ </> _ = impossible


castRel :: Path' s (Rel d1) a -> Path' s (Rel d2) a
---- System
castRel (RelDir p pr)   = RelDir p pr
castRel (RelFile p pr)  = RelFile p pr
---- Windows
castRel (RelDirW p pr)  = RelDirW p pr
castRel (RelFileW p pr) = RelFileW p pr
---- Posix
castRel (RelDirP p pr)  = RelDirP p pr
castRel (RelFileP p pr) = RelFileP p pr
castRel _               = impossible

castDir :: Path' s a (Dir d1) -> Path' s a (Dir d2)
---- System
castDir (AbsDir p)     = AbsDir p
castDir (RelDir p pr)  = RelDir p pr
---- Windows
castDir (AbsDirW p)    = AbsDirW p
castDir (RelDirW p pr) = RelDirW p pr
---- Posix
castDir (AbsDirP p)    = AbsDirP p
castDir (RelDirP p pr) = RelDirP p pr
castDir _              = impossible

-- TODO: I was not able to unite these two functions (`relDirToPosix` and `relFileToPosix`) into just `toPosix``
--   because Haskell did not believe me that I would be returning same "t" (Dir/File) in Path
--   as was in first argument. I wonder if there is easy way to go around that or if
--   we have to redo significant part of the StrongPath to be able to do smth like this.
-- | Converts relative path to posix by replacing current path separators with posix path separators.
--   Works well for "normal" relative paths like "a\b\c" (Win) or "a/b/c" (Posix).
--   If path is weird but still considered relative, like just "C:" on Win,
--   results can be unxpected, most likely resulting with error thrown.
--   If path is already Posix, it will not change.
relDirToPosix :: MonadThrow m => Path' s (Rel d1) (Dir d2) -> m (Path' Posix (Rel d1) (Dir d2))
relDirToPosix sp@(RelDir _ _)  = parseRelDirP $ FPP.joinPath $ FP.splitDirectories $ toFilePath sp
relDirToPosix sp@(RelDirW _ _) = parseRelDirP $ FPP.joinPath $ FPW.splitDirectories $ toFilePath sp
relDirToPosix (RelDirP p pr)   = return $ RelDirP p pr
relDirToPosix _                = impossible
relFileToPosix :: MonadThrow m => Path' s (Rel d1) (File' f) -> m (Path' Posix (Rel d1) (File' f))
relFileToPosix sp@(RelFile _ _)  = parseRelFileP $ FPP.joinPath $ FP.splitDirectories $ toFilePath sp
relFileToPosix sp@(RelFileW _ _) = parseRelFileP $ FPP.joinPath $ FPW.splitDirectories $ toFilePath sp
relFileToPosix (RelFileP p pr)   = return $ RelFileP p pr
relFileToPosix _                 = impossible
-- TODO: Should I name these unsafe versions differently? Maybe relDirToPosixU?
-- Unsafe versions:
relDirToPosix' :: Path' s (Rel d1) (Dir d2) -> Path' Posix (Rel d1) (Dir d2)
relDirToPosix' = fromJust . relDirToPosix
relFileToPosix' :: Path' s (Rel d1) (File' f) -> Path' Posix (Rel d1) (File' f)
relFileToPosix' = fromJust . relFileToPosix
