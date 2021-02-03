module Lib
    ( compile
    , ProjectRootDir
    , DataDir
    ) where

import           Control.Monad       (unless, when)
import           Gasp                (Attr (..), Gasp (..), GaspElement (..),
                                      Metric (..))
import           Generator           (writeAppCode)
import           Generator.Common    (ProjectRootDir)
import           Generator.Templates (DataDir)
import           Parser              (parseGasp)
import           StrongPath          (Abs, Dir, File, Path, toFilePath)
import           Text.Printf         (printf)
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
          when (attrScale x <= 0) $ gaspError $ concat
            [ "[error] attr %s: " `printf` attrName x
            , "except scale > 0, but got "
            , "scale=%f ," `printf` attrScale x
            ]
          when (attrMin x >= attrMax x) $ gaspError $ concat
            [ "[error] attr %s: " `printf` attrName x
            , "except min < max, but got "
            , "min=%f ," `printf` attrMin x
            , "max=%f ," `printf` attrMax x
            ]
          unless valid $ gaspWarn $ concat
            [ "[warning] attr %s: " `printf` attrName x
            , "default is %f " `printf` attrDef x
            , "not in [%f, " `printf` attrMin x
            , "%f], " `printf` attrMax x
            , "use %f" `printf` defv
            ]
          return $ GaspElementAttr x {attrDef = defv}
          where (valid, defv) = getCenterValue (attrMin x, attrMax x) (attrDef x)
        mapFunc (GaspElementMetric x) = do
          when (metricPrec x < 1) $ gaspError $ concat
            [ "[error] metric %s: " `printf` metricName x
            , "except prec > 0, but got "
            , "prec=%d ," `printf` metricPrec x
            ]
          when (metricMin x >= metricMax x) $ gaspError $ concat
            [ "[error] metric %s: " `printf` metricName x
            , "except min < max, but got "
            , "min=%f ," `printf` metricMin x
            , "max=%f ," `printf` metricMax x
            ]
          when (metricMinThreshold x >= metricMaxThreshold x) $ gaspError $ concat
            [ "[error] metric %s: " `printf` metricName x
            , "except min_threshold < max_threshold, but got "
            , "min_threshold=%f ," `printf` metricMinThreshold x
            , "max_threshold=%f ," `printf` metricMaxThreshold x
            ]
          unless valid $ gaspWarn $ concat
            [ "[warning] metric %s: " `printf` metricName x
            , "threshold is %f " `printf` metricThreshold x
            , "not in [%f, " `printf` metricMinThreshold x
            , "%f], " `printf` metricMaxThreshold x
            , "use %f" `printf` defv
            ]
          return $ GaspElementMetric x {metricThreshold = defv}
          where (valid, defv) = getCenterValue (metricMinThreshold x, metricMaxThreshold x) (metricThreshold x)
        mapFunc v             = return v

getCenterValue :: (Ord a) => (a, a) -> a -> (Bool, a)
getCenterValue (minv, maxv) defv =
  ((defv >= minv && defv <= maxv), max minv (min maxv defv))

gaspError :: String -> IO ()
gaspError what = putStrLn $ Term.applyStyles [Term.Red] what

gaspWarn :: String -> IO ()
gaspWarn what = putStrLn $ Term.applyStyles [Term.Cyan] what
