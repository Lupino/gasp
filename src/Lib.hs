module Lib
    ( compile
    , ProjectRootDir
    , DataDir
    ) where

import           Control.Monad       (unless)
import           Gasp                (Attr (..), Gasp (..), GaspElement (..),
                                      Metric (..))
import           Generator           (writeAppCode)
import           Generator.Common    (ProjectRootDir)
import           Generator.Templates (DataDir)
import           Parser              (parseGasp)
import           StrongPath          (Abs, Dir, File, Path, toFilePath)
import qualified Util.Terminal       as Term


type CompileError = String

compile :: Path Abs File -> Path Abs (Dir ProjectRootDir) -> Path Abs (Dir DataDir) -> IO (Either CompileError ())
compile gaspFile outDir dataDir = do
    gaspStr <- readFile (toFilePath gaspFile)

    case parseGasp gaspStr of
        Left err   -> return $ Left (show err)
        Right gasp0 -> do
          gasp <- preprocessGasp gasp0
          generateCode gasp
  where
    generateCode gasp = writeAppCode gasp outDir dataDir >> return (Right ())


preprocessGasp :: Gasp -> IO Gasp
preprocessGasp gasp = Gasp <$> mapM mapFunc (gaspElements gasp)
  where mapFunc :: GaspElement -> IO GaspElement
        mapFunc (GaspElementAttr x)   = do
          unless valid $ gaspSays $ concat
            [ "[warning] attr "
            , attrName x
            , " default is "
            , show (attrDef x)
            , " not in ["
            , show (attrMin x)
            , ", "
            , show (attrMax x)
            , "], use "
            , show defv
            ]
          return $ GaspElementAttr x {attrDef = defv}
          where (valid, defv) = getCenterValue (attrMin x, attrMax x) (attrDef x)
        mapFunc (GaspElementMetric x) = do
          unless valid $ gaspSays $ concat
            [ "[warning] metric "
            , metricName x
            , " threshold is "
            , show (metricThreshold x)
            , " not in ["
            , show (metricMinThreshold x)
            , ", "
            , show (metricMaxThreshold x)
            , "], use "
            , show defv
            ]
          return $ GaspElementMetric x {metricThreshold = defv}
          where (valid, defv) = getCenterValue (metricMinThreshold x, metricMaxThreshold x) (metricThreshold x)
        mapFunc v             = return v

getCenterValue :: (Ord a) => (a, a) -> a -> (Bool, a)
getCenterValue (minv, maxv) defv =
  ((defv >= minv && defv <= maxv), max minv (min maxv defv))

gaspSays :: String -> IO ()
gaspSays what = putStrLn $ Term.applyStyles [Term.Red, Term.Yellow] what
