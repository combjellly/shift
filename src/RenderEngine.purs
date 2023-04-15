module RenderEngine where

import AST
import Parser

import Prelude
import Data.Int (toNumber,ceil)
import Data.List
import Data.Either
import Control.Plus (empty)
import Tempo
import Data.DateTime
import Prelude 
import Data.Newtype
import Data.Time.Duration
import Data.Maybe
import Data.DateTime.Instant
import Data.Map
import Data.Int



--EventBank for troubleshooting
type EventBank = (List Event) 
data Event = Number
type Cycles = Number


type SampleEvent = 
  {
  s :: String,
  n :: Int,
  whenPosix :: Number
  ,gain:: Number
  ,note:: Number
  ,pan:: Number
  ,cut:: Int
  }

sampleNaming :: Tempo -> String -> Number -> Number -> Number -> Number -> Number -> Int -> SampleEvent
sampleNaming t s wCycles sampleNum pitch g p cutGroup= {s:s, n:(round sampleNum), whenPosix:(unwrap (unInstant (fromDateTime (countToTime t wCycles))))/1000.0, gain:g, note:pitch, pan:p, cut:cutGroup} 
