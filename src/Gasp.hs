module Gasp
    ( Gasp
    , Expr (..)
    , fromGaspExprs
    , setGaspExprs
    , getGaspExprs
    , setProd
    , getProd
    , getRequires

    , module Gasp.App
    , module Gasp.Attr
    , module Gasp.Metric
    , module Gasp.Require

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
import           Gasp.AGpio
import           Gasp.App
import           Gasp.Attr
import           Gasp.Command
import           Gasp.Constant
import           Gasp.Every
import           Gasp.Flag
import           Gasp.Function
import           Gasp.Gpio
import           Gasp.Import
import           Gasp.Init
import           Gasp.Loop
import           Gasp.Metric
import           Gasp.Require
import           Gasp.Rule
import           Gasp.Setup
import           Gasp.Timer
import           Gasp.Uart


-- * Gasp

data Gasp = Gasp
    { gaspExprs         :: ![Expr]
    , externalCodeFiles :: ![ExternalCode.File]
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
    | ExprAGpio !AGpio
    | ExprRule !Rule
    | ExprConst !Constant
    | ExprUart !Uart
    | ExprRequire !Require
    | ExprImport !Import
    | ExprTimer !Timer
    deriving (Show, Eq)

fromGaspExprs :: [Expr] -> Gasp
fromGaspExprs exprs = Gasp
    { gaspExprs         = exprs
    , externalCodeFiles = []
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


hasFloatMetric :: [Metric] -> Bool
hasFloatMetric [] = False
hasFloatMetric (x:xs)
  | isFloatMetric x = True
  | otherwise     = hasFloatMetric xs

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
  | isInput (gpioBind x) = True
  | otherwise = hasInput xs


-- * AGpios

getAGpios :: Gasp -> [AGpio]
getAGpios gasp = [agpio | (ExprAGpio agpio) <- gaspExprs gasp]


-- * Uarts

getUarts :: Gasp -> [Uart]
getUarts gasp = [uart | (ExprUart uart) <- gaspExprs gasp]


-- * Requires

getRequires :: Gasp -> [Require]
getRequires gasp = [r | (ExprRequire r) <- gaspExprs gasp]


-- * Imports

getImports :: Gasp -> [Import]
getImports gasp = [imp | (ExprImport imp) <- gaspExprs gasp]


-- * Timer

getTimers :: Gasp -> [Timer]
getTimers gasp = [r | (ExprTimer r) <- gaspExprs gasp]


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
getCommandLength (ExprTimer t)   = setTimerLength t
getCommandLength _               = 0


getMaxCommandLength :: Gasp -> Int
getMaxCommandLength = maximum . map getCommandLength . gaspExprs

getRequestValueLength :: Expr -> Int
getRequestValueLength (ExprAttr attr) = getAttrValueLength attr
getRequestValueLength (ExprMetric m)  = getMetricValueLength m
getRequestValueLength (ExprTimer _)   = 10
getRequestValueLength _               = 0


getMaxRequestValueLength :: Gasp -> Int
getMaxRequestValueLength = maximum . map getRequestValueLength . gaspExprs

getTmplLength :: Expr -> Int
getTmplLength (ExprCmd cmd)   = getCmdRspLength cmd
getTmplLength (ExprAttr attr) = getAttrRspLength attr
getTmplLength (ExprMetric m)  = getMetricThresholdRspLength m
getTmplLength (ExprTimer _)   = timerRspLength
getTmplLength _               = 0


getMaxTmplLength :: Gasp -> Int
getMaxTmplLength = maximum . map getTmplLength . gaspExprs


prepareGasp :: Int -> [Flag] -> Gasp -> Gasp
prepareGasp sAddr flags gasp = setGaspExprs gasp . go 1 sAddr $ gaspExprs gasp
  where go :: Int -> Int -> [Expr] -> [Expr]
        go _ _ []        = []
        go ri addr (ExprAttr x:xs)
          | attrKeep x = ExprAttr x {attrAddr = addr} : go ri (addr + getAttrDataLength x) xs
          | otherwise  = ExprAttr x : go ri addr xs
        go ri addr (ExprMetric x:xs)
          | metricAuto x = ExprMetric x {metricAddr = addr} : go ri (addr + getMetricDataLength x) xs
          | otherwise  = ExprMetric x : go ri addr xs
        go ri addr (ExprTimer x:xs) = ExprTimer x {timerAddr = addr} : go ri (addr + timerDataLen) xs
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
        , "imports"     .= getImports gasp
        , "loops"       .= getLoops gasp
        , "setups"      .= getSetups gasp
        , "inits"       .= inits
        , "attrs"       .= attrs
        , "has_attr"    .= hasAttr
        , "metrics"     .= metrics
        , "has_metric"  .= hasMetric
        , "max_req_len" .= (getMaxRequestValueLength gasp + 1)
        , "max_buf_len" .= (bufLen + 1)
        , "max_tpl_len" .= (maxTmplLen + 1)
        , "max_gl_len"  .= (contextLen + bufLen + 1)
        , "actions"     .= getEverys gasp
        , "gpios"       .= gpios
        , "agpios"      .= agpios
        , "rules"       .= rules
        , "has_gpio"    .= not (null gpios)
        , "has_input"   .= hasInput gpios
        , "has_debug"   .= constDebug consts
        , "has_rule"    .= not (null rules)
        , "has_float"   .= (hasFloatAttr attrs || hasFloatMetric metrics)
        , "low_memory"  .= isLowMemory
        , "consts"      .= consts
        , "uarts"       .= uarts
        , "has_uart"    .= not (null uarts)
        , "ctrl_mode"   .= ctrlMode
        , "production"  .= prod
        , "timers"      .= timers
        , "has_timer"   .= hasTimer
        , "auto_retry"  .= maybe True appRetry app
        ]
        where gasp = prepareGasp (maybe 0 (startAddr prod) app) (getFlags gasp0) gasp0
              prod = getProd gasp0
              attrs = getAttrs gasp
              metrics = getMetrics gasp
              gpios = getGpios gasp
              agpios = getAGpios gasp
              uarts = getUarts gasp
              cmds = getCmds gasp
              funcs = getFunctions gasp
              inits = getInits gasp
              consts = getConstants gasp
              hasMetric = not (null metrics)
              hasAttr = not (null attrs)
              app = getApp gasp0
              rules = getRules gasp
              timers = getTimers gasp
              hasTimer = not (null timers)
              maxCmdLen = getMaxCommandLength gasp
              maxTmplLen = getMaxTmplLength gasp
              bufLen0 = getTotalMetricThresholdLength (getTotalAttrLength 0 attrs) metrics
              isLowMemory = maybe False appLowMemory app
              bufLen = if isLowMemory then max maxCmdLen maxTmplLen else max maxCmdLen bufLen0
              contextLen = maybe 0 appContexLen app
              ctrlMode = maybe False appCtrl app

putExpr :: Expr -> Put
putExpr (ExprAttr x)
  | attrKeep x && isFloatAttr x = putFloatle . realToFrac $ attrDef x * attrScale x
  | attrKeep x = putInt32le . floor $ attrDef x * attrScale x
  | otherwise  = return ()
putExpr (ExprMetric x)
  | metricAuto x && isFloatMetric x = putFloatle . realToFrac $ metricThreshold x
  | metricAuto x = putInt32le . floor $ metricThreshold x
  | otherwise  = return ()
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
