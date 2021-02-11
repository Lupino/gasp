module Gasp
    ( Gasp
    , Expr (..)
    , fromGaspExprs
    , setGaspExprs
    , getGaspExprs

    , module Gasp.Attr
    , module Gasp.Metric

    , setExternalCodeFiles
    , getExternalCodeFiles
    ) where

import           Data.Aeson     (ToJSON (..), object, (.=))
import qualified ExternalCode
import           Gasp.App
import           Gasp.Attr
import           Gasp.Command
import           Gasp.Every
import           Gasp.Flag
import           Gasp.Function
import           Gasp.Gpio
import           Gasp.Init
import           Gasp.Loop
import           Gasp.Metric
import           Gasp.Rule
import           Gasp.Setup
import           Gasp.Telemetry


-- * Gasp

data Gasp = Gasp
    { gaspExprs         :: [Expr]
    , externalCodeFiles :: [ExternalCode.File]
    } deriving (Show, Eq)

data Expr
    = ExprApp !App
    | ExprCmd !Command
    | ExprTelemetry !Telemetry
    | ExprFunction !Function
    | ExprInit !Init
    | ExprSetup !Setup
    | ExprLoop !Loop
    | ExprAttr !Attr
    | ExprMetric !Metric
    | ExprEvery !Every
    | ExprGpio !Gpio
    | ExprRule !Rule
    deriving (Show, Eq)

fromGaspExprs :: [Expr] -> Gasp
fromGaspExprs exprs = Gasp
    { gaspExprs = exprs
    , externalCodeFiles = []
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

-- * App

getApp :: Gasp -> App
getApp gasp =
  case apps of
    [app] -> app
    []    -> emptyApp
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

-- * Telemetries

getTelemetries:: Gasp -> [Telemetry]
getTelemetries gasp = [t | (ExprTelemetry t) <- gaspExprs gasp]

-- * Loops

getLoops:: Gasp -> [Loop]
getLoops gasp = [loop | (ExprLoop loop) <- gaspExprs gasp]

-- * Setups

getSetups:: Gasp -> [Setup]
getSetups gasp = [setup | (ExprSetup setup) <- gaspExprs gasp]

-- * Inits

getInits:: Gasp -> [Init]
getInits gasp = [initv | (ExprInit initv) <- gaspExprs gasp]

initDebug :: [Init] -> Bool
initDebug [] = False
initDebug (x:xs)
  | hasToken "DEBUG_SERIAL" (initCode x) = True
  | otherwise = initDebug xs

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


setTelemetryFlag :: [Flag] -> Telemetry -> Telemetry
setTelemetryFlag flags telem = telem
  { telemFlag = getFlag flags (telemFlag telem)
  }


getCommandLength :: Expr -> Int
getCommandLength (ExprCmd cmd)   = length $ cmdFunc cmd
getCommandLength (ExprAttr attr) = length (attrName attr) + 4
getCommandLength (ExprMetric m)  = length (metricName m) + 17
getCommandLength _               = 0

getMaxCommandLength :: Gasp -> Int
getMaxCommandLength = maximum . map getCommandLength . gaspExprs


prepareGasp :: [Flag] -> Gasp -> Gasp
prepareGasp flags = fromGaspExprs . go 1 . gaspExprs
  where go :: Int -> [Expr] -> [Expr]
        go _ []        = []
        go addr (ExprAttr x:xs) = ExprAttr x {attrAddr = addr} : go (addr + 4) xs
        go addr (ExprMetric x:xs) = ExprMetric x {metricAddr = addr} : go (addr + 4) xs
        go addr (ExprTelemetry x:xs) = ExprTelemetry (setTelemetryFlag flags x) : go addr xs
        go addr (ExprCmd x:xs) = ExprCmd (setCommandFlag flags x) : go addr xs
        go addr (ExprFunction x:xs) = ExprFunction (setFunctionFlag flags x) : go addr xs
        go addr (x:xs) = x : go addr xs

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
collectFlags flags (ExprTelemetry x:xs)
  | telemFlag x `elem` flags = collectFlags flags xs
  | otherwise = collectFlags (telemFlag x : flags) xs
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
        [ "app"         .= getApp gasp
        , "commands"    .= cmds
        , "telemetries" .= telems
        , "functions"   .= funcs
        , "loops"       .= getLoops gasp
        , "setups"      .= getSetups gasp
        , "inits"       .= inits
        , "attrs"       .= attrs
        , "has_attr"    .= hasAttr
        , "metrics"     .= metrics
        , "has_metric"  .= (hasMetric || hasTelems)
        , "use_eeprom"  .= useEeprom
        , "max_cmd_len" .= (getMaxCommandLength gasp + 1)
        , "actions"     .= getEverys gasp
        , "gpios"       .= gpios
        , "rules"       .= getRules gasp
        , "has_gpio"    .= not (null gpios)
        , "has_func"    .= not (null funcs)
        , "has_input"   .= hasInput gpios
        , "use_remote"  .= (useEeprom || hasCmd)
        , "has_debug"   .= initDebug inits
        ]
        where gasp = prepareGasp (getFlags gasp0) gasp0
              attrs = getAttrs gasp
              metrics = getMetrics gasp
              telems  = getTelemetries gasp
              gpios = getGpios gasp
              cmds = getCmds gasp
              funcs = getFunctions gasp
              inits = getInits gasp
              hasMetric = not (null metrics)
              hasTelems = not (null telems)
              hasAttr = not (null attrs)
              useEeprom = hasMetric || hasAttr
              hasCmd = not (null cmds)
