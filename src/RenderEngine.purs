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
  s :: String
  ,n :: Int
  ,whenPosix :: Number
  ,gain:: Number
  ,note:: Number
  ,pan:: Number
  ,cut:: Int
  ,velocity :: Number
  ,channel :: Number
  ,duration :: Number
  }

type MidiEvent = 

  {
    channel :: Number,
    whenPosix :: Number,
    note :: Number,
    velocity :: Number,
    duration :: Number
  }

-- sampleMidiNaming :: Tempo -> String -> Number -> Number -> Number -> Number -> Number -> Int -> Number -> Number -> SampleEvent

midiEventHandler :: Tempo -> Number -> Number -> Number -> Number -> Number -> MidiEvent
midiEventHandler t c wCycles n v d = {channel : c, whenPosix: (whenPosixTime t wCycles), note: n, velocity: v, duration: d}

sampleNaming :: Tempo -> String -> Number -> Number -> Number -> Number -> Number -> Int -> Number -> Number -> Number -> SampleEvent
sampleNaming t s wCycles sampleNum pitch g p cutGroup channel velocity d= {s:s, n:(round sampleNum), whenPosix: (whenPosixTime t wCycles), gain:g, note:pitch, pan:p, cut:cutGroup, channel:channel, velocity:velocity, duration: d} 

whenPosixTime :: Tempo -> Number -> Number 
whenPosixTime t wCycles = (unwrap (unInstant (fromDateTime (countToTime t wCycles))))/1000.0
