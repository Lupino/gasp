module Gasp
    ( Gasp
    , Expr (..)
    , fromGaspExprs
    , setGaspExprs
    , getGaspExprs
    , setProd
    , getProd
    , setArgvFlags
    , getRequires
    , filterFlag

    , module Gasp.App
    , module Gasp.Attr
    , module Gasp.Metric
    , module Gasp.Require

    , setExternalCodeFiles
    , getExternalCodeFiles
    ) where

import           Data.Aeson             (ToJSON (..), object, (.=))
import           Data.Binary            (Binary (..))
import           Data.Binary.Put        (Put, putByteString, putFloatle,
                                         putInt32le)
import           Data.ByteString.Base16 as B16 (decodeLenient)
import           Data.ByteString.Char8  as BC (pack)
import           Data.List              (nub)
import           Data.Maybe             (isJust)
import           Data.Text              (Text)
import qualified Data.Text              as T (intercalate, pack)
import qualified ExternalCode
import           Gasp.AGpio
import           Gasp.App
import           Gasp.Attr
import           Gasp.Block
import           Gasp.Command
import           Gasp.Constant
import           Gasp.Every
import           Gasp.Flag
import           Gasp.Function
import           Gasp.Gpio
import           Gasp.Import
import           Gasp.Metric
import           Gasp.Require
import           Gasp.Rule
import           Gasp.Timer
import           Gasp.Uart


-- * Gasp

data Gasp = Gasp
    { gaspExprs         :: ![Expr]
    , externalCodeFiles :: ![ExternalCode.File]
    , isProd            :: !Bool
    , argvFlags         :: ![Flag]
    } deriving (Show, Eq)

data Expr
    = ExprApp      !App
    | ExprCmd      !Command
    | ExprFunction !Function
    | ExprSetup    !Setup
    | ExprLoop     !Loop
    | ExprRaw      !Raw
    | ExprAttr     !Attr
    | ExprMetric   !Metric
    | ExprEvery    !Every
    | ExprGpio     !Gpio
    | ExprAGpio    !AGpio
    | ExprRule     !Rule
    | ExprConst    !Constant
    | ExprUart     !Uart
    | ExprRequire  !Require
    | ExprImport   !Import
    | ExprTimer    !Timer
    | ExprFlag     !Flag
    deriving (Show, Eq)

fromGaspExprs :: [Expr] -> Gasp
fromGaspExprs exprs = Gasp
    { gaspExprs         = exprs
    , externalCodeFiles = []
    , isProd            = False
    , argvFlags         = []
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

-- * ArgvFlags

setArgvFlags :: Gasp -> [Flag] -> Gasp
setArgvFlags gasp flags = gasp { argvFlags = flags }

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
getFunctions gasp = nub $ [func | (ExprFunction func) <- gaspExprs gasp]

-- * Loops

getLoops:: Gasp -> [Loop]
getLoops gasp = nub $ [loop | (ExprLoop loop) <- gaspExprs gasp]

-- * Setups

getSetups:: Gasp -> [Setup]
getSetups gasp = nub $ [setup | (ExprSetup setup) <- gaspExprs gasp]

-- * Raws

getRaws:: Gasp -> [Raw]
getRaws gasp = nub $ [raw | (ExprRaw raw) <- gaspExprs gasp]

-- * Attrs

getAttrs:: Gasp -> [Attr]
getAttrs gasp = [attr | (ExprAttr attr) <- gaspExprs gasp]

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
getConstants gasp = nub $ [c | (ExprConst c) <- gaspExprs gasp]

-- * Gpios

getGpios :: Gasp -> [Gpio]
getGpios gasp = [gpio | (ExprGpio gpio) <- gaspExprs gasp]


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


-- * Timers

getTimers :: Gasp -> [Timer]
getTimers gasp = [r | (ExprTimer r) <- gaspExprs gasp]


-- * Flags

getFlags :: Gasp -> [Flag]
getFlags gasp = [r | (ExprFlag r) <- gaspExprs gasp]


-- * FuncFlags

getFuncFlags:: Gasp -> [FuncFlag]
getFuncFlags gasp = map (`guessFuncFlag` exprs) (collectFuncFlags [] exprs)
  where exprs = gaspExprs gasp


getFuncFlag :: [FuncFlag] -> FuncFlag -> FuncFlag
getFuncFlag [] flag = flag
getFuncFlag (x:xs) flag
  | x == flag = x
  | otherwise = getFuncFlag xs flag


setFunctionFlag :: [FuncFlag] -> Function -> Function
setFunctionFlag flags func = func
  { funcFlag = getFuncFlag flags (funcFlag func)
  }


setCommandFlag :: [FuncFlag] -> Command -> Command
setCommandFlag flags cmd = cmd
  { cmdFlag = getFuncFlag flags (cmdFlag cmd)
  }


getCommandLength :: Expr -> Int
getCommandLength (ExprCmd cmd)   = length $ cmdName cmd
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


prepareGasp :: Int -> [FuncFlag] -> Gasp -> Gasp
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

guessFuncFlag :: FuncFlag -> [Expr] -> FuncFlag
guessFuncFlag flag [] = flag
guessFuncFlag flag (ExprFunction x:xs)
  | funcFlag x == flag = flag
    { flagRetval = hasRetval x
    , flagJson = hasJson x
    }
  | otherwise = guessFuncFlag flag xs
guessFuncFlag flag (_:xs) = guessFuncFlag flag xs

collectFuncFlags :: [FuncFlag] -> [Expr] -> [FuncFlag]
collectFuncFlags flags [] = flags
collectFuncFlags flags (ExprCmd x:xs)
  | cmdFlag x `elem` flags = collectFuncFlags flags xs
  | otherwise = collectFuncFlags (cmdFlag x : flags) xs
collectFuncFlags flags (ExprFunction x:xs)
  | funcFlag x `elem` flags = collectFuncFlags flags xs
  | otherwise = collectFuncFlags (funcFlag x : flags) xs
collectFuncFlags flags (_:xs) = collectFuncFlags flags xs

-- * ToJSON instances.

instance ToJSON Gasp where
    toJSON gasp0 = object $
        [ "app"         .= app
        , "has_app"     .= isJust app
        , "commands"    .= cmds
        , "functions"   .= requiredFuncs
        , "imports"     .= getImports gasp
        , "loops"       .= loops
        , "setups"      .= setups
        , "raws"        .= raws
        , "attrs"       .= attrs
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
        , "consts"      .= reverse requiredConsts
        , "vars"        .= reverse requiredVars
        , "uarts"       .= uarts
        , "production"  .= prod
        , "timers"      .= timers
        , "has_timer"   .= hasTimer
        ] ++ map (\(Flag k v) -> k .= v) flags
        where gasp = prepareGasp (maybe 0 (startAddr prod) app) (getFuncFlags gasp0) gasp0
              setups = getSetups gasp
              loops = getLoops gasp
              raws = getRaws gasp
              prod = getProd gasp0
              flags = nub $ argvFlags gasp0 ++ getFlags gasp ++ defaultFlags
              attrs = getAttrs gasp
              metrics = getMetrics gasp
              gpios = getGpios gasp
              agpios = getAGpios gasp
              uarts = getUarts gasp
              cmds = getCmds gasp
              (consts, vars) = splitConstant $ getConstants gasp
              requiredText
                =  T.intercalate "\n"
                $  map loopCode loops
                ++ map setupCode setups
                ++ map rawCode raws
              funcs = getFunctions gasp
              (requiredVars, requiredConsts, requiredFuncs) = getRequired requiredText vars consts funcs
              hasMetric = not (null metrics)
              app = getApp gasp0
              rules = getRules gasp
              timers = getTimers gasp
              hasTimer = not (null timers)
              maxCmdLen = getMaxCommandLength gasp
              maxTmplLen = getMaxTmplLength gasp
              bufLen0 = getTotalMetricThresholdLength (getTotalAttrLength 0 attrs) metrics
              isLowMemory = getFlag False flags "low_memory"
              bufLen = if isLowMemory then max maxCmdLen maxTmplLen else max maxCmdLen bufLen0
              contextLen = maybe 0 appContexLen app

getRequired :: Text -> [Constant] -> [Constant] -> [Function] -> ([Constant], [Constant], [Function])
getRequired requiredText vars consts funcs
  | null requiredVars && null requiredConsts && null requiredFuncs = ([], [], [])
  | otherwise = (requiredVars ++ rv, requiredConsts ++ rc, requiredFuncs ++ rf)
  where (requiredFuncs, unrequiredFuncs) = getRequiredFunction requiredText funcs
        (requiredVars, unrequiredVars) = getRequiredConstant requiredText vars
        (requiredConsts, unrequiredConsts) = getRequiredConstant requiredText consts
        txt
          = T.intercalate "\n"
          $ map (T.pack . constValue) requiredConsts
          ++ map (\(Constant a b c) -> T.pack $ concat [a, " ", b, " ", c]) requiredVars
          ++ map funcCode requiredFuncs
        (rv, rc, rf) = getRequired txt unrequiredVars unrequiredConsts unrequiredFuncs

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
    putByteString $ B16.decodeLenient $ BC.pack token
    putByteString $ B16.decodeLenient $ BC.pack addr
    mapM_ putExpr exprs
    where app = getApp gasp
          token = maybe "1234567890abcdef" appToken app
          addr = maybe "00000000" appAddr app

  put Gasp {gaspExprs=exprs} = mapM_ putExpr exprs


filterFlag :: Gasp -> Gasp
filterFlag gasp = setGaspExprs gasp [ExprFlag flag | ExprFlag flag <- gaspExprs gasp]
