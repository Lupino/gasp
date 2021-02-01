module Gasp
    ( Gasp
    , GaspElement (..)
    , fromGaspElems

    , module Gasp.App
    , fromApp
    , getApp
    , setApp

    , module Gasp.Command
    , getCmds
    , addCmd

    , module Gasp.Function
    , getFunctions
    , addFunction

    , module Gasp.Telemetry
    , getTelemetries
    , addTelemetry

    , module Gasp.Init
    , getInits
    , addInit

    , module Gasp.Setup
    , getSetups
    , addSetup

    , module Gasp.Loop
    , getLoops
    , addLoop

    , module Gasp.Flag
    , getFlags
    , addFlag

    , module Gasp.Attr
    , getAttrs
    , addAttr

    , module Gasp.Metric
    , getMetrics
    , addMetric

    , module Gasp.Monitor
    , getMonitors
    , addMonitor
    ) where

import           Data.Aeson     (ToJSON (..), object, (.=))

import           Gasp.App
import           Gasp.Attr
import           Gasp.Command
import           Gasp.Flag
import           Gasp.Function
import           Gasp.Init
import           Gasp.Loop
import           Gasp.Metric
import           Gasp.Monitor
import           Gasp.Setup
import           Gasp.Telemetry


-- * Gasp

data Gasp = Gasp
    { gaspElements      :: [GaspElement]
    } deriving (Show, Eq)

data GaspElement
    = GaspElementApp !App
    | GaspElementCmd !Command
    | GaspElementTelemetry !Telemetry
    | GaspElementFunction !Function
    | GaspElementInit !Init
    | GaspElementSetup !Setup
    | GaspElementLoop !Loop
    | GaspElementFlag !Flag
    | GaspElementAttr !Attr
    | GaspElementMetric !Metric
    | GaspElementMonitor !Monitor
    deriving (Show, Eq)

fromGaspElems :: [GaspElement] -> Gasp
fromGaspElems elems = Gasp
    { gaspElements = elems
    }

-- * App

getApp :: Gasp -> App
getApp gasp = let apps = getApps gasp in
    if (length apps /= 1)
    then error "Gasp has to contain exactly one GaspElementApp element!"
    else head apps

isAppElem :: GaspElement -> Bool
isAppElem GaspElementApp{} = True
isAppElem _                = False

getApps :: Gasp -> [App]
getApps gasp = [app | (GaspElementApp app) <- gaspElements gasp]

setApp :: Gasp -> App -> Gasp
setApp gasp app = gasp { gaspElements = (GaspElementApp app) : (filter (not . isAppElem) (gaspElements gasp)) }

fromApp :: App -> Gasp
fromApp app = fromGaspElems [GaspElementApp app]

-- * Commands

getCmds :: Gasp -> [Command]
getCmds gasp = [cmd | (GaspElementCmd cmd) <- gaspElements gasp]

addCmd :: Gasp -> Command -> Gasp
addCmd gasp cmd = gasp { gaspElements = (GaspElementCmd cmd):(gaspElements gasp) }

-- * Functions

getFunctions:: Gasp -> [Function]
getFunctions gasp = [func | (GaspElementFunction func) <- gaspElements gasp]

addFunction :: Gasp -> Function -> Gasp
addFunction gasp func = gasp { gaspElements = (GaspElementFunction func):(gaspElements gasp) }

-- * Telemetries

getTelemetries:: Gasp -> [Telemetry]
getTelemetries gasp = [t | (GaspElementTelemetry t) <- gaspElements gasp]

addTelemetry :: Gasp -> Telemetry -> Gasp
addTelemetry gasp t = gasp { gaspElements = (GaspElementTelemetry t):(gaspElements gasp) }

-- * Loops

getLoops:: Gasp -> [Loop]
getLoops gasp = [loop | (GaspElementLoop loop) <- gaspElements gasp]

addLoop :: Gasp -> Loop -> Gasp
addLoop gasp loop = gasp { gaspElements = (GaspElementLoop loop):(gaspElements gasp) }

-- * Setups

getSetups:: Gasp -> [Setup]
getSetups gasp = [setup | (GaspElementSetup setup) <- gaspElements gasp]

addSetup :: Gasp -> Setup -> Gasp
addSetup gasp setup = gasp { gaspElements = (GaspElementSetup setup):(gaspElements gasp) }

-- * Inits

getInits:: Gasp -> [Init]
getInits gasp = [initv | (GaspElementInit initv) <- gaspElements gasp]

addInit :: Gasp -> Init -> Gasp
addInit gasp initv = gasp { gaspElements = (GaspElementInit initv):(gaspElements gasp) }

-- * Attrs

getAttrs:: Gasp -> [Attr]
getAttrs gasp = [attr | (GaspElementAttr attr) <- gaspElements gasp]

addAttr :: Gasp -> Attr -> Gasp
addAttr gasp attr = gasp { gaspElements = (GaspElementAttr attr):(gaspElements gasp) }

-- * Metrics

getMetrics:: Gasp -> [Metric]
getMetrics gasp = [metric | (GaspElementMetric metric) <- gaspElements gasp]

addMetric :: Gasp -> Metric -> Gasp
addMetric gasp metric = gasp { gaspElements = (GaspElementMetric metric):(gaspElements gasp) }

-- * Monitors

getMonitors:: Gasp -> [Monitor]
getMonitors gasp = [mon | (GaspElementMonitor mon) <- gaspElements gasp]

addMonitor :: Gasp -> Monitor -> Gasp
addMonitor gasp mon = gasp { gaspElements = (GaspElementMonitor mon):(gaspElements gasp) }

-- * Flags

getFlags:: Gasp -> [Flag]
getFlags gasp = [flag | (GaspElementFlag flag) <- gaspElements gasp]

addFlag :: Gasp -> Flag -> Gasp
addFlag gasp flag = gasp { gaspElements = (GaspElementFlag flag):(gaspElements gasp) }

getFlag :: [Flag] -> Flag -> Flag
getFlag [] flag = flag
getFlag (x:xs) flag
  | flagFunc x == flagFunc flag = x
  | otherwise = getFlag xs flag


setFunctionFlag :: [Flag] -> Function -> Function
setFunctionFlag flags func = func
  { funcFlag = getFlag flags (funcFlag func) {flagFunc = funcName func}
  }


setCommandFlag :: [Flag] -> Command -> Command
setCommandFlag flags cmd = cmd
  { cmdFlag = getFlag flags (cmdFlag cmd) {flagFunc = cmdFunc cmd}
  }


setTelemetryFlag :: [Flag] -> Telemetry -> Telemetry
setTelemetryFlag flags telem = telem
  { telemFlag = getFlag flags (telemFlag telem) {flagFunc = telemFunc telem}
  }


setAttrsAddr :: Int -> [Attr] -> [Attr]
setAttrsAddr _ [] = []
setAttrsAddr addr (x:xs) = x { attrAddr = show addr }: setAttrsAddr (addr + 4) xs


setMetricsAddr :: Int -> [Metric] -> [Metric]
setMetricsAddr _ [] = []
setMetricsAddr addr (x:xs) = x { metricAddr = show addr }: setMetricsAddr (addr + 4) xs


getCommandLength :: GaspElement -> Int
getCommandLength (GaspElementCmd cmd)   = length $ cmdFunc cmd
getCommandLength (GaspElementAttr attr) = length (attrName attr) + 4
getCommandLength (GaspElementMetric m)  = length (metricName m) + 17
getCommandLength _                      = 0

getMaxCommandLength :: Gasp -> Int
getMaxCommandLength = maximum . map getCommandLength . gaspElements


-- * ToJSON instances.

instance ToJSON Gasp where
    toJSON gasp = object
        [ "app"         .= getApp gasp
        , "commands"    .= map (setCommandFlag flags) (getCmds gasp)
        , "telemetries" .= map (setTelemetryFlag flags) (getTelemetries gasp)
        , "functions"   .= map (setFunctionFlag flags) (getFunctions gasp)
        , "loops"       .= getLoops gasp
        , "setups"      .= getSetups gasp
        , "inits"       .= getInits gasp
        , "attrs"       .= setAttrsAddr startAttrAddr attrs
        , "has_attr"    .= (length attrs > 0)
        , "metrics"     .= setMetricsAddr startMetricAddr metrics
        , "has_metric"  .= (length metrics > 0)
        , "use_eeprom"  .= (length metrics > 0 || length attrs > 0)
        , "max_cmd_len" .= (getMaxCommandLength gasp + 1)
        , "monitors"    .= getMonitors gasp
        ]
        where flags = getFlags gasp
              attrs = getAttrs gasp
              metrics = getMetrics gasp
              startAttrAddr = 1
              startMetricAddr = startAttrAddr + (length attrs * 4)
