module Gasp.Timer
  ( Timer (..)
  , timerDataLen
  , timerRspLength
  , setTimerLength
  ) where

import           Data.Aeson    (ToJSON (..), object, (.=))
import           Gasp.Function (FuncName)


data Timer = Timer
  { timerName :: !String -- Identifier
  , timerFn0  :: !FuncName
  , timerFn1  :: !FuncName
  , timerAddr :: Int
  } deriving (Show, Eq)

instance ToJSON Timer where
    toJSON timer = object
      [ "name"  .= timerName timer
      , "fn0"   .= timerFn0 timer
      , "fn1"   .= timerFn1 timer
      , "addr0" .= addr0
      , "addr1" .= addr1
      , "addr2" .= addr2
      ]
      where addr0 = timerAddr timer
            addr1 = addr0 + 4
            addr2 = addr1 + 4
            -- sched_at     什么时候执行  0 为禁用      uint32    4
            -- period       周期          0 为执行一次  uint32    4
            -- duration     持续时间      0 为一直执行  uint32    4

timerDataLen :: Int
timerDataLen = 4 + 4 + 4

timerNameLen :: Timer -> Int
timerNameLen = length . timerName

-- {"sched_at": 1617503208, "period": 1617503208, "duration": 1617503208}
timerRspLength :: Int
timerRspLength = 70

--           sched_at   period     duration
-- {"data": "1617503208,1617503208,1617503208", "method": "set_timer", "name": "vv"}
setTimerLength :: Timer -> Int
setTimerLength = (80 +) . timerNameLen
