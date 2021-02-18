module Lib
    ( compile
    , ProjectRootDir
    , TemplateDir
    ) where

import           CompileOptions        (CompileOptions)
import qualified CompileOptions
import           Control.Monad         (unless)
import qualified Data.ByteString.Char8 as BC (putStrLn)
import           Data.Yaml             (encode)
import qualified ExternalCode
import           Gasp                  (Attr (..), Expr (..), Gasp, Metric (..),
                                        getGaspExprs, setExternalCodeFiles,
                                        setGaspExprs, setLowMemory)
import           Generator             (writeAppCode)
import           Generator.Common      (ProjectRootDir)
import           Generator.Template    (TemplateDir)
import           Parser                (parseGasp)
import           StrongPath            (Abs, File, Path, toFilePath)
import           Text.Printf           (printf)
import qualified Util.Terminal         as Term


type CompileError = String

compile
  :: Path Abs File
  -> CompileOptions
  -> IO (Either CompileError ())
compile gaspFile options = do
    gaspStr <- readFile (toFilePath gaspFile)

    case parseGasp gaspStr of
        Left err    -> return $ Left (show err)
        Right gasp ->
          enrichGaspASTBasedOnCompileOptions gasp options
            >>= preprocessGasp >>= generateCode (CompileOptions.showSyntaxTree options)
  where
    generateCode False gasp = writeAppCode gasp outDir tempDir >> return (Right ())
    generateCode True gasp  = BC.putStrLn (encode gasp) >> return (Right ())
    outDir = CompileOptions.projectRootDir options
    tempDir = CompileOptions.templateDir options


preprocessGasp :: Gasp -> IO Gasp
preprocessGasp gasp = setGaspExprs gasp <$> mapM mapFunc (getGaspExprs gasp)
  where mapFunc :: Expr -> IO Expr
        mapFunc (ExprAttr x)   = do
          unless (attrScale x > 0) $ gaspError $ concat
            [ "[error] attr %s: " `printf` attrName x
            , "except scale > 0, but got "
            , "scale=%f ," `printf` attrScale x
            ]
          unless (attrMin x < attrMax x) $ gaspError $ concat
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
          return $ ExprAttr x {attrDef = defv}
          where (valid, defv) = getCenterValue (attrMin x, attrMax x) (attrDef x)
        mapFunc (ExprMetric x) = do
          unless (metricPrec x > 0) $ gaspError $ concat
            [ "[error] metric %s: " `printf` metricName x
            , "except prec > 0, but got "
            , "prec=%d ," `printf` metricPrec x
            ]
          unless (metricMin x < metricMax x) $ gaspError $ concat
            [ "[error] metric %s: " `printf` metricName x
            , "except min < max, but got "
            , "min=%f ," `printf` metricMin x
            , "max=%f ," `printf` metricMax x
            ]
          unless (metricMinThreshold x < metricMaxThreshold x) $ gaspError $ concat
            [ "[error] metric %s: " `printf` metricName x
            , "except min_threshold < max_threshold, but got "
            , "min_threshold=%f ," `printf` metricMinThreshold x
            , "max_threshold=%f ," `printf` metricMaxThreshold x
            ]
          unless (metricMinThreshold x > 0) $ gaspError $ concat
            [ "[error] metric %s: " `printf` metricName x
            , "except min_threshold > 0, but got "
            , "min_threshold=%f ," `printf` metricMinThreshold x
            ]
          unless (metricMaxThreshold x > 0) $ gaspError $ concat
            [ "[error] metric %s: " `printf` metricName x
            , "except max_threshold > 0, but got "
            , "max_threshold=%f ," `printf` metricMaxThreshold x
            ]
          unless valid $ gaspWarn $ concat
            [ "[warning] metric %s: " `printf` metricName x
            , "threshold is %f " `printf` metricThreshold x
            , "not in [%f, " `printf` metricMinThreshold x
            , "%f], " `printf` metricMaxThreshold x
            , "use %f" `printf` defv
            ]
          return $ ExprMetric x {metricThreshold = defv}
          where (valid, defv) = getCenterValue (metricMinThreshold x, metricMaxThreshold x) (metricThreshold x)
        mapFunc v             = return v

getCenterValue :: (Ord a) => (a, a) -> a -> (Bool, a)
getCenterValue (minv, maxv) defv =
  (defv >= minv && defv <= maxv, max minv (min maxv defv))

gaspError :: String -> IO ()
gaspError what = putStrLn $ Term.applyStyles [Term.Red] what

gaspWarn :: String -> IO ()
gaspWarn what = putStrLn $ Term.applyStyles [Term.Cyan] what

enrichGaspASTBasedOnCompileOptions :: Gasp -> CompileOptions -> IO Gasp
enrichGaspASTBasedOnCompileOptions gasp options = do
    externalCodeFiles <- ExternalCode.readFiles (CompileOptions.externalCodeDirPath options)
    return (gasp `setExternalCodeFiles` externalCodeFiles `setLowMemory` CompileOptions.lowMemory options)
