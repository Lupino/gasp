module Gasp
    ( Gasp
    , GaspElement (..)
    , fromGaspElems
    , setGaspElems
    , getGaspElems

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
import           Gasp.Init
import           Gasp.Loop
import           Gasp.Metric
import           Gasp.Setup
import           Gasp.Telemetry


-- * Gasp

data Gasp = Gasp
    { gaspElements      :: [GaspElement]
    , externalCodeFiles :: [ExternalCode.File]
    } deriving (Show, Eq)

data GaspElement
    = GaspElementApp !App
    | GaspElementCmd !Command
    | GaspElementTelemetry !Telemetry
    | GaspElementFunction !Function
    | GaspElementInit !Init
    | GaspElementSetup !Setup
    | GaspElementLoop !Loop
    | GaspElementAttr !Attr
    | GaspElementMetric !Metric
    | GaspElementEvery !Every
    deriving (Show, Eq)

fromGaspElems :: [GaspElement] -> Gasp
fromGaspElems elems = Gasp
    { gaspElements = elems
    , externalCodeFiles = []
    }

setGaspElems :: Gasp -> [GaspElement] -> Gasp
setGaspElems gasp elems = gasp {gaspElements = elems}

getGaspElems :: Gasp -> [GaspElement]
getGaspElems = gaspElements

-- * External code files

getExternalCodeFiles :: Gasp -> [ExternalCode.File]
getExternalCodeFiles = externalCodeFiles

setExternalCodeFiles :: Gasp -> [ExternalCode.File] -> Gasp
setExternalCodeFiles wasp files = wasp { externalCodeFiles = files }

-- * App

getApp :: Gasp -> App
getApp gasp = let apps = getApps gasp in
    if length apps /= 1
    then error "Gasp has to contain exactly one GaspElementApp element!"
    else head apps

getApps :: Gasp -> [App]
getApps gasp = [app | (GaspElementApp app) <- gaspElements gasp]

-- * Commands

getCmds :: Gasp -> [Command]
getCmds gasp = [cmd | (GaspElementCmd cmd) <- gaspElements gasp]

-- * Functions

getFunctions:: Gasp -> [Function]
getFunctions gasp = [func | (GaspElementFunction func) <- gaspElements gasp]

-- * Telemetries

getTelemetries:: Gasp -> [Telemetry]
getTelemetries gasp = [t | (GaspElementTelemetry t) <- gaspElements gasp]

-- * Loops

getLoops:: Gasp -> [Loop]
getLoops gasp = [loop | (GaspElementLoop loop) <- gaspElements gasp]

-- * Setups

getSetups:: Gasp -> [Setup]
getSetups gasp = [setup | (GaspElementSetup setup) <- gaspElements gasp]

-- * Inits

getInits:: Gasp -> [Init]
getInits gasp = [initv | (GaspElementInit initv) <- gaspElements gasp]

-- * Attrs

getAttrs:: Gasp -> [Attr]
getAttrs gasp = [attr | (GaspElementAttr attr) <- gaspElements gasp]

-- * Metrics

getMetrics:: Gasp -> [Metric]
getMetrics gasp = [metric | (GaspElementMetric metric) <- gaspElements gasp]

-- * Everys

getEverys:: Gasp -> [Every]
getEverys gasp = [every | (GaspElementEvery every) <- gaspElements gasp]

-- * Flags

getFlags:: Gasp -> [Flag]
getFlags gasp = map (`guessFlag` elems) (collectFlags [] elems)
  where elems = gaspElements gasp


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


getCommandLength :: GaspElement -> Int
getCommandLength (GaspElementCmd cmd)   = length $ cmdFunc cmd
getCommandLength (GaspElementAttr attr) = length (attrName attr) + 4
getCommandLength (GaspElementMetric m)  = length (metricName m) + 17
getCommandLength _                      = 0

getMaxCommandLength :: Gasp -> Int
getMaxCommandLength = maximum . map getCommandLength . gaspElements


prepareGasp :: [Flag] -> Gasp -> Gasp
prepareGasp flags = fromGaspElems . go 1 . gaspElements
  where go :: Int -> [GaspElement] -> [GaspElement]
        go _ []        = []
        go addr (GaspElementAttr x:xs) = GaspElementAttr x {attrAddr = addr} : go (addr + 4) xs
        go addr (GaspElementMetric x:xs) = GaspElementMetric x {metricAddr = addr} : go (addr + 4) xs
        go addr (GaspElementTelemetry x:xs) = GaspElementTelemetry (setTelemetryFlag flags x) : go addr xs
        go addr (GaspElementCmd x:xs) = GaspElementCmd (setCommandFlag flags x) : go addr xs
        go addr (GaspElementFunction x:xs) = GaspElementFunction (setFunctionFlag flags x) : go addr xs
        go addr (x:xs) = x : go addr xs

guessFlag :: Flag -> [GaspElement] -> Flag
guessFlag flag [] = flag
guessFlag flag (GaspElementFunction x:xs)
  | funcFlag x == flag = flag
    { flagRetval = hasRetval x
    , flagJson = hasJson x
    }
  | otherwise = guessFlag flag xs
guessFlag flag (_:xs) = guessFlag flag xs

collectFlags :: [Flag] -> [GaspElement] -> [Flag]
collectFlags flags [] = flags
collectFlags flags (GaspElementTelemetry x:xs)
  | telemFlag x `elem` flags = collectFlags flags xs
  | otherwise = collectFlags (telemFlag x : flags) xs
collectFlags flags (GaspElementCmd x:xs)
  | cmdFlag x `elem` flags = collectFlags flags xs
  | otherwise = collectFlags (cmdFlag x : flags) xs
collectFlags flags (GaspElementFunction x:xs)
  | funcFlag x `elem` flags = collectFlags flags xs
  | otherwise = collectFlags (funcFlag x : flags) xs
collectFlags flags (_:xs) = collectFlags flags xs

-- * ToJSON instances.

instance ToJSON Gasp where
    toJSON gasp0 = object
        [ "app"         .= getApp gasp
        , "commands"    .= getCmds gasp
        , "telemetries" .= telems
        , "functions"   .= getFunctions gasp
        , "loops"       .= getLoops gasp
        , "setups"      .= getSetups gasp
        , "inits"       .= getInits gasp
        , "attrs"       .= attrs
        , "has_attr"    .= not (null attrs)
        , "metrics"     .= metrics
        , "has_metric"  .= (not (null metrics) || not (null telems))
        , "use_eeprom"  .= (not (null metrics) || not (null attrs))
        , "max_cmd_len" .= (getMaxCommandLength gasp + 1)
        , "actions"     .= getEverys gasp
        ]
        where gasp = prepareGasp (getFlags gasp0) gasp0
              attrs = getAttrs gasp
              metrics = getMetrics gasp
              telems  = getTelemetries gasp
