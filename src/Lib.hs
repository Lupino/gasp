module Lib
  ( compile
  ) where


import           CompileOptions             (CompileOptions, CompileType (..))
import qualified CompileOptions
import           Control.Monad              (unless)
import           Data.Aeson                 (toJSON)
import qualified Data.Binary                as Bin (encode)
import qualified Data.ByteString.Char8      as BC (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as BL (writeFile)
import           Data.Text                  (Text)
import qualified Data.Text                  as T (unpack)
import           Data.UUID                  (toString)
import           Data.UUID.V4               (nextRandom)
import           Data.Yaml                  (encode)
import qualified ExternalCode
import           Gasp                       (App (..), Attr (..), Expr (..),
                                             Gasp, Metric (..), getFlags,
                                             getGaspExprs, getTmpl, getTmpls,
                                             setArgvFlags, setExternalCodeFiles,
                                             setGaspExprs, setProd)
import           Gasp.Block
import           Gasp.Function
import           Generator                  (writeAppCode)
import           Generator.Template         (compileAndRenderTextTemplate)
import           Parser                     (parseGasp, parseGasp0)
import           System.FilePath            ((</>))
import           Text.Printf                (printf)
import qualified Util.Terminal              as Term


type CompileError = String

compile
  :: FilePath
  -> CompileOptions
  -> IO (Either CompileError ())
compile gaspFile options = do
    r <- parseGasp tempDir gaspFile

    case r of
        Left err    -> return . Left $ show err
        Right gasp -> do
          Right r0 <- parseGasp tempDir $ tempDir </> "stage1/constants.gasp"
          let gasp1 = setGaspExprs gasp (getGaspExprs gasp ++ getGaspExprs r0)
          enrichGaspASTBasedOnCompileOptions gasp1 options
            >>= preprocessGasp >>= generateCode (CompileOptions.compileType options)
  where
    generateCode Compile gasp = writeAppCode gasp outDir tempDir >> return (Right ())
    generateCode Syntax gasp  = BC.putStrLn (encode gasp) >> return (Right ())
    generateCode Eeprom gasp  = BL.writeFile eepFile (Bin.encode gasp) >> return (Right ())
    outDir = CompileOptions.projectRootDir options
    tempDir = CompileOptions.templateDir options
    eepFile = outDir </> "eeprom.bin"


preprocessGasp :: Gasp -> IO Gasp
preprocessGasp gasp = setGaspExprs gasp . foldr foldFunc [] <$> mapM mapFunc (getGaspExprs gasp)
  where mapFunc :: Expr -> IO Expr
        mapFunc (ExprAttr x)   = do
          unless (attrScale x > 0) $ gaspError $ concat
            [ "[error] attr %s: " `printf` show (attrName x)
            , "except scale > 0, but got "
            , "scale=%f ," `printf` attrScale x
            ]
          unless (attrMin x < attrMax x) $ gaspError $ concat
            [ "[error] attr %s: " `printf` show (attrName x)
            , "except min < max, but got "
            , "min=%f ," `printf` attrMin x
            , "max=%f ," `printf` attrMax x
            ]
          unless valid $ gaspWarn $ concat
            [ "[warning] attr %s: " `printf` show (attrName x)
            , "default is %f " `printf` attrDef x
            , "not in [%f, " `printf` attrMin x
            , "%f], " `printf` attrMax x
            , "use %f" `printf` defv
            ]
          return $ ExprAttr x {attrDef = defv}
          where (valid, defv) = getCenterValue (attrMin x, attrMax x) (attrDef x)
        mapFunc (ExprMetric x) = do
          unless (metricPrec x > 0) $ gaspError $ concat
            [ "[error] metric %s: " `printf` show (metricName x)
            , "except prec > 0, but got "
            , "prec=%d ," `printf` metricPrec x
            ]
          unless (metricMin x < metricMax x) $ gaspError $ concat
            [ "[error] metric %s: " `printf` show (metricName x)
            , "except min < max, but got "
            , "min=%f ," `printf` metricMin x
            , "max=%f ," `printf` metricMax x
            ]
          unless (metricMinThreshold x < metricMaxThreshold x) $ gaspError $ concat
            [ "[error] metric %s: " `printf` show (metricName x)
            , "except min_threshold < max_threshold, but got "
            , "min_threshold=%f ," `printf` metricMinThreshold x
            , "max_threshold=%f ," `printf` metricMaxThreshold x
            ]
          unless (metricMinThreshold x > 0) $ gaspError $ concat
            [ "[error] metric %s: " `printf` show (metricName x)
            , "except min_threshold > 0, but got "
            , "min_threshold=%f ," `printf` metricMinThreshold x
            ]
          unless (metricMaxThreshold x > 0) $ gaspError $ concat
            [ "[error] metric %s: " `printf` show (metricName x)
            , "except max_threshold > 0, but got "
            , "max_threshold=%f ," `printf` metricMaxThreshold x
            ]
          unless valid $ gaspWarn $ concat
            [ "[warning] metric %s: " `printf` show (metricName x)
            , "threshold is %f " `printf` metricThreshold x
            , "not in [%f, " `printf` metricMinThreshold x
            , "%f], " `printf` metricMaxThreshold x
            , "use %f" `printf` defv
            ]
          return $ ExprMetric x {metricThreshold = defv}
          where (valid, defv) = getCenterValue (metricMinThreshold x, metricMaxThreshold x) (metricThreshold x)

        mapFunc (ExprApp app@App{appToken=""}) = do
          token <-  filter (/='-') . toString <$> nextRandom
          return $ ExprApp app {appToken = token}
        mapFunc (ExprSetup (Setup n code)) = return . ExprSetup . Setup n $ render code
        mapFunc (ExprLoop (Loop n code)) = return . ExprLoop . Loop n $ render code
        mapFunc (ExprSetup1 (Setup1 n code)) = return . ExprSetup1 . Setup1 n $ render code
        mapFunc (ExprLoop1 (Loop1 n code)) = return . ExprLoop1 . Loop1 n $ render code
        mapFunc (ExprRaw (Raw n code)) = return . ExprRaw . Raw n $ render code
        mapFunc (ExprRender (Render n)) = doRender n render
        mapFunc (ExprRender1 (Render1 n v)) = doRender n (`compileAndRenderTextTemplate` v)
        mapFunc (ExprFunction func) = return $ ExprFunction func
          { funcCode = render $ funcCode func
          }
        mapFunc (ExprIfEq (IfEq n code)) =
          if ifFlag n then doRenderBlock n code render
                      else pure (ExprRendered [])
        mapFunc (ExprIfNeq (IfNeq n code)) =
          if ifFlag n then pure (ExprRendered [])
                      else doRenderBlock n code render

        mapFunc v = return v

        render =  (`compileAndRenderTextTemplate` toJSON gasp)

        tmpls = getTmpls gasp
        flags = getFlags gasp

        ifFlag n = getFlag False flags n

        doRender :: String -> (Text -> Text) -> IO Expr
        doRender n r =
          case getTmpl n tmpls of
            Nothing -> error $ Term.applyStyles [Term.Red] $ "Inline template " ++ n ++ " not found."
            Just (Tmpl _ code) -> doRenderBlock n code r

        doRenderBlock :: String -> Text -> (Text -> Text) -> IO Expr
        doRenderBlock n code r =
          case parseGasp0 n (T.unpack (r code) ++ "\n") of
            Left e      -> error $ Term.applyStyles [Term.Red] $ show e
            Right exprs -> return $ ExprRendered exprs

        foldFunc :: Expr -> [Expr] -> [Expr]
        foldFunc (ExprRendered xs) acc = xs ++ acc
        foldFunc e acc                 = e:acc

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
    return (gasp `setExternalCodeFiles` externalCodeFiles
                 `setProd` CompileOptions.isProd options
                 `setArgvFlags` CompileOptions.argvFlags options)
