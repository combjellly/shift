module Tempo where

import Data.DateTime
import Prelude 
import Data.Newtype
import Data.Time.Duration
import Data.Maybe


type Tempo = DateTime

-- time to count is elapsed cycles. 
timeToCount :: Tempo -> DateTime -> Number
timeToCount launchDateTime x = unwrap (diff x launchDateTime :: Seconds) * 2.0

countToTime :: Tempo -> Number -> DateTime
countToTime launchDateTime c =  
  let nSeconds = Seconds $ c/2.0  --Seconds
      newTime = adjust nSeconds launchDateTime -- Maybe DateTime
  in case newTime of
    Nothing -> launchDateTime
    Just x -> x 

