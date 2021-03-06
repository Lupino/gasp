module Gasp
    ( Gasp
    , Expr (..)
    , fromGaspExprs
    , setGaspExprs
    , getGaspExprs
    , setLowMemory
    , getLowMemory
    , setProd
    , getProd

    , module Gasp.App
    , module Gasp.Attr
    , module Gasp.Metric

    , setExternalCodeFiles
    , getExternalCodeFiles
    ) where

import           Data.Aeson            (ToJSON (..), object, (.=))
import           Data.Binary           (Binary (..))
import           Data.Binary.Put       (Put, putByteString, putFloatle,
                                        putInt32le)
import           Data.ByteString.Char8 as BC (pack)
import           Data.HexString        (hexString, toBytes)
import           Data.Maybe            (isJust)
import qualified ExternalCode
import           Gasp.App
import           Gasp.Attr
import           Gasp.Command
import           Gasp.Constant
import           Gasp.Every
import           Gasp.Flag
import           Gasp.Function
import           Gasp.Gpio
import           Gasp.Init
import           Gasp.Loop
import           Gasp.Metric
import           Gasp.Rule
import           Gasp.Setup


-- * Gasp

data Gasp = Gasp
    { gaspExprs         :: ![Expr]
    , externalCodeFiles :: ![ExternalCode.File]
    , isLowMemory       :: !Bool
    , isProd            :: !Bool
    } deriving (Show, Eq)

data Expr
    = ExprApp !App
    | ExprCmd !Command
    | ExprFunction !Function
    | ExprInit !Init
    | ExprSetup !Setup
    | ExprLoop !Loop
    | ExprAttr !Attr
    | ExprMetric !Metric
    | ExprEvery !Every
    | ExprGpio !Gpio
    | ExprRule !Rule
    | ExprConst !Constant
    deriving (Show, Eq)

fromGaspExprs :: [Expr] -> Gasp
fromGaspExprs exprs = Gasp
    { gaspExprs         = exprs
    , externalCodeFiles = []
    , isLowMemory       = False
    , isProd            = False
    }

setGaspExprs :: Gasp -> [Expr] -> Gasp
setGaspExprs gasp exprs = gasp {gaspExprs = exprs}

getGaspExprs :: Gasp -> [Expr]
getGaspExprs = gaspExprs

-- * External code files

getExternalCodeFiles :: Gasp -> [ExternalCode.File]
getExternalCodeFiles = externalCodeFiles

setExternalCodeFiles :: Gasp -> [ExternalCode.File] -> Gasp
setExternalCodeFiles wasp files = wasp { externalCodeFiles = files }

-- * Low Memory

setLowMemory :: Gasp -> Bool -> Gasp
setLowMemory gasp lowMem = gasp { isLowMemory = lowMem }

getLowMemory :: Gasp -> Bool
getLowMemory = isLowMemory

-- * Production

setProd :: Gasp -> Bool -> Gasp
setProd gasp prod = gasp { isProd = prod }

getProd :: Gasp -> Bool
getProd = isProd

-- * App

getApp :: Gasp -> Maybe App
getApp gasp =
  case apps of
    [app] -> Just app
    []    -> Nothing
    _     -> error "Gasp has to contain exactly one ExprApp element!"

  where apps = getApps gasp

getApps :: Gasp -> [App]
getApps gasp = [app | (ExprApp app) <- gaspExprs gasp]

-- * Commands

getCmds :: Gasp -> [Command]
getCmds gasp = [cmd | (ExprCmd cmd) <- gaspExprs gasp]

-- * Functions

getFunctions:: Gasp -> [Function]
getFunctions gasp = [func | (ExprFunction func) <- gaspExprs gasp]

-- * Loops

getLoops:: Gasp -> [Loop]
getLoops gasp = [loop | (ExprLoop loop) <- gaspExprs gasp]

-- * Setups

getSetups:: Gasp -> [Setup]
getSetups gasp = [setup | (ExprSetup setup) <- gaspExprs gasp]

-- * Inits

getInits:: Gasp -> [Init]
getInits gasp = [initv | (ExprInit initv) <- gaspExprs gasp]

constDebug :: [Constant] -> Bool
constDebug [] = False
constDebug (x:xs)
  | constName x == "DEBUG_SERIAL" = True
  | otherwise = constDebug xs

-- * Attrs

getAttrs:: Gasp -> [Attr]
getAttrs gasp = [attr | (ExprAttr attr) <- gaspExprs gasp]


hasFloatAttr :: [Attr] -> Bool
hasFloatAttr [] = False
hasFloatAttr (x:xs)
  | isFloatAttr x = True
  | otherwise     = hasFloatAttr xs

-- * Metrics

getMetrics:: Gasp -> [Metric]
getMetrics gasp = [metric | (ExprMetric metric) <- gaspExprs gasp]

-- * Everys

getEverys:: Gasp -> [Every]
getEverys gasp = [every | (ExprEvery every) <- gaspExprs gasp]

-- * Rules

getRules:: Gasp -> [Rule]
getRules gasp = [rule | (ExprRule rule) <- gaspExprs gasp]

-- * Consts

getConstants:: Gasp -> [Constant]
getConstants gasp = [c | (ExprConst c) <- gaspExprs gasp]

-- * Gpios

getGpios :: Gasp -> [Gpio]
getGpios gasp = [gpio | (ExprGpio gpio) <- gaspExprs gasp]

hasInput :: [Gpio] -> Bool
hasInput [] = False
hasInput (x:xs)
  | null (gpioFunc x) = hasInput xs
  | otherwise = True

-- * Flags

getFlags:: Gasp -> [Flag]
getFlags gasp = map (`guessFlag` exprs) (collectFlags [] exprs)
  where exprs = gaspExprs gasp


getFlag :: [Flag] -> Flag -> Flag
getFlag [] flag = flag
getFlag (x:xs) flag
  | x == flag = x
  | otherwise = getFlag xs flag


setFunctionFlag :: [Flag] -> Function -> Function
setFunctionFlag flags func = func
  { funcFlag = getFlag flags (funcFlag func)
  }


setCommandFlag :: [Flag] -> Command -> Command
setCommandFlag flags cmd = cmd
  { cmdFlag = getFlag flags (cmdFlag cmd)
  }


getCommandLength :: Expr -> Int
getCommandLength (ExprCmd cmd)   = length $ cmdFunc cmd
getCommandLength (ExprAttr attr) = setAttrLength attr
getCommandLength (ExprMetric m)  = setMetricThresholdLength m
getCommandLength _               = 0


getMaxCommandLength :: Gasp -> Int
getMaxCommandLength = maximum . map getCommandLength . gaspExprs

getRequestValueLength :: Expr -> Int
getRequestValueLength (ExprAttr attr) = getAttrValueLength attr
getRequestValueLength (ExprMetric m)  = getMetricValueLength m
getRequestValueLength _               = 0


getMaxRequestValueLength :: Gasp -> Int
getMaxRequestValueLength = maximum . map getRequestValueLength . gaspExprs

getTmplLength :: Expr -> Int
getTmplLength (ExprCmd cmd)   = getCmdRspLength cmd
getTmplLength (ExprAttr attr) = getAttrRspLength attr
getTmplLength (ExprMetric m)  = getMetricThresholdRspLength m
getTmplLength _               = 0


getMaxTmplLength :: Gasp -> Int
getMaxTmplLength = maximum . map getTmplLength . gaspExprs


prepareGasp :: Int -> [Flag] -> Gasp -> Gasp
prepareGasp sAddr flags gasp = setGaspExprs gasp . go 1 sAddr $ gaspExprs gasp
  where go :: Int -> Int -> [Expr] -> [Expr]
        go _ _ []        = []
        go ri addr (ExprAttr x:xs)
          | attrKeep x = ExprAttr x {attrAddr = addr} : go ri (addr + 4) xs
          | otherwise  = ExprAttr x : go ri addr xs
        go ri addr (ExprMetric x:xs) = ExprMetric x {metricAddr = addr} : go ri (addr + 4) xs
        go ri addr (ExprCmd x:xs) = ExprCmd (setCommandFlag flags x) : go ri addr xs
        go ri addr (ExprFunction x:xs) = ExprFunction (setFunctionFlag flags x) : go ri addr xs
        go ri addr (ExprRule x:xs) = ExprRule x {ruleIndex=ri} : go (ri + 1) addr xs
        go ri addr (x:xs) = x : go ri addr xs

guessFlag :: Flag -> [Expr] -> Flag
guessFlag flag [] = flag
guessFlag flag (ExprFunction x:xs)
  | funcFlag x == flag = flag
    { flagRetval = hasRetval x
    , flagJson = hasJson x
    }
  | otherwise = guessFlag flag xs
guessFlag flag (_:xs) = guessFlag flag xs

collectFlags :: [Flag] -> [Expr] -> [Flag]
collectFlags flags [] = flags
collectFlags flags (ExprCmd x:xs)
  | cmdFlag x `elem` flags = collectFlags flags xs
  | otherwise = collectFlags (cmdFlag x : flags) xs
collectFlags flags (ExprFunction x:xs)
  | funcFlag x `elem` flags = collectFlags flags xs
  | otherwise = collectFlags (funcFlag x : flags) xs
collectFlags flags (_:xs) = collectFlags flags xs

-- * ToJSON instances.

instance ToJSON Gasp where
    toJSON gasp0 = object
        [ "app"         .= app
        , "has_app"     .= isJust app
        , "commands"    .= cmds
        , "functions"   .= funcs
        , "loops"       .= getLoops gasp
        , "setups"      .= getSetups gasp
        , "inits"       .= inits
        , "attrs"       .= attrs
        , "has_attr"    .= hasAttr
        , "metrics"     .= metrics
        , "has_metric"  .= hasMetric
        , "use_eeprom"  .= useEeprom
        , "max_req_len" .= (getMaxRequestValueLength gasp + 1)
        , "max_buf_len" .= (bufLen + 1)
        , "max_tpl_len" .= (maxTmplLen + 1)
        , "max_gl_len"  .= (contextLen + bufLen + 1)
        , "actions"     .= getEverys gasp
        , "gpios"       .= gpios
        , "rules"       .= rules
        , "has_gpio"    .= not (null gpios)
        , "has_func"    .= (hasFunc || useEeprom)
        , "has_input"   .= hasInput gpios
        , "has_debug"   .= constDebug consts
        , "has_rule"    .= not (null rules)
        , "has_float"   .= (hasFloatAttr attrs || hasMetric)
        , "low_memory"  .= getLowMemory gasp
        , "consts"      .= consts
        , "ctrl_mode"   .= ctrlMode
        , "production"  .= prod
        ]
        where gasp = prepareGasp (maybe 0 (startAddr prod) app) (getFlags gasp0) gasp0
              prod = getProd gasp0
              attrs = getAttrs gasp
              metrics = getMetrics gasp
              gpios = getGpios gasp
              cmds = getCmds gasp
              funcs = getFunctions gasp
              hasFunc = not (null funcs)
              inits = getInits gasp
              consts = getConstants gasp
              hasMetric = not (null metrics)
              hasAttr = not (null attrs)
              useEeprom = hasMetric || hasAttr
              app = getApp gasp0
              rules = getRules gasp
              maxCmdLen = getMaxCommandLength gasp
              maxTmplLen = getMaxTmplLength gasp
              bufLen0 = getTotalMetricThresholdLength (getTotalAttrLength 0 attrs) metrics
              bufLen = if getLowMemory gasp then max maxCmdLen maxTmplLen else max maxCmdLen bufLen0
              contextLen = maybe 0 appContexLen app
              ctrlMode = maybe False appCtrl app

putExpr :: Expr -> Put
putExpr (ExprAttr x)
  | attrKeep x && isFloatAttr x = putFloatle . realToFrac $ attrDef x * attrScale x
  | attrKeep x = putInt32le . floor $ attrDef x * attrScale x
  | otherwise  = return ()
putExpr (ExprMetric x) = putFloatle . realToFrac $ metricThreshold x
putExpr _ = return ()

instance Binary Gasp where
  get = error "not implement"
  put gasp@Gasp {isProd=True, gaspExprs=exprs} = do
    putByteString $ toBytes $ hexString $ BC.pack token
    putByteString $ toBytes $ hexString $ BC.pack addr
    mapM_ putExpr exprs
    where app = getApp gasp
          token = maybe "1234567890ABCDEF" appToken app
          addr = maybe "00000000" appAddr app

  put Gasp {gaspExprs=exprs} = mapM_ putExpr exprs
