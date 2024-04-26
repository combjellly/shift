module Main where
import Parser
import AST
import Tempo
import RenderEngine


import Prelude

import Data.Either
import Data.Maybe
import Parsing
import Data.Identity
import RenderEngine
import Effect
import Data.List
import Effect.Class.Console
import Control.Monad.Unlift
import Effect.Aff
import Data.Time.Duration
import Effect.Class (liftEffect)
import Effect.Ref
import Effect.Random
import Data.Enum
import Effect.Now (nowDateTime)
import Data.DateTime
import Data.Time.Duration 
import Data.Newtype (unwrap)
import Data.Maybe
import Data.Tuple
import WebDirt
import Data.Traversable
import Data.Map as Map
import Data.Int (toNumber,ceil,fromString,round)

type EngineRecord = 
  {
    programRef :: Ref Program
  , tempo :: Ref Tempo -- Number 
  , wStart :: Ref DateTime
  , wEnd :: Ref DateTime
  , variables :: Ref (Map.Map String Number)
  , originalVariables :: Ref (Map.Map String Number)
  , sequences :: Ref (Map.Map String (List Number))

  }

main :: Effect Unit 
main = pure unit

--launched from js end. provides a base engine record 
launch :: Effect EngineRecord 
launch = do 
  launchDateTime <- nowDateTime
  tempo <- new $ launchDateTime 
  wStart <- new $ launchDateTime
  wEnd <- new $ launchDateTime
  programRef <- new $ LoopElement (Loop (Constant 2.0) (Play "dogTest...woof?" (Constant 3.0) ({gain :Constant 50.0,note :Constant  0.0, pan : Constant 50.0, cut : Constant 0.0}):Nil)):Nil
  variables <- new $ Map.singleton "default" 0.0
  originalVariables <- new $ Map.singleton "default" 0.0
  sequences <- new $ Map.singleton "default" Nil

  -- create emptyMap. function already for this. 
  let er = {programRef,tempo,wStart,wEnd,variables,originalVariables,sequences}
  -- _ <- launchAff $ crudeScheduler 0.03 er
  pure er

--launched from js end. provides a wd instance
launchDirt :: Effect WebDirt
launchDirt = do
  wd <- newWebDirt { sampleMapUrl: "samples/sampleMap.json", sampleFolder: "samples" }
  initializeWebAudio wd
  pure wd

-- 
parse :: EngineRecord -> String -> Effect String
parse er x = case (runParser x program) of
    Left err -> pure $ show err 
    Right p -> do 
        write p er.programRef
        pure $ show "success"

renderStandalone :: EngineRecord -> WebDirt -> Effect String
renderStandalone er wd = do 
  t <- nowDateTime 
  prevW <- read $ er.wEnd 
  let futureTime = fromMaybe t $ adjust (Milliseconds 400.00) t -- :: Milliseconds
  if prevW <= futureTime then do
    let wS = prevW
    let wE = fromMaybe t $ adjust (Milliseconds 500.0) wS 
    write wS er.wStart
    write wE er.wEnd
    t <- read $ er.tempo
    let cycleStart = timeToCount t wS
    let cycleEnd = timeToCount t wE
    p <- liftEffect $ read $ er.programRef
    variables <- liftEffect $ read $ er.variables

    se <- whatDoINeedToDoThisRenderFrame er p t cycleStart cycleEnd -- read and write variable refs + renderEvents 
    play wd se
    pure $ show se

  else
    pure $ show "sleepy time"

{-
splitEvents :: List (Either SampleEvent MidiEvent) -> (List SampleEvent, List MidiEvent)
splitEvents events = foldr go ([], []) events
  where
    go (Right midi) (samples, midis) = (samples, midi : midis)
    go (Left sample) (samples, midis) = (sample : samples, midis)

handlePlayMidi :: Either SampleEvent MidiEvent -> WebDirt -> Effect String
handlePlayMidi (Right midi) wd = do
    pure $ show midi

handlePlayMidi (Left p) wd = do
     play wd p
     pure $ show "Nil"

whatDoINeedToDoThisMIDIRenderFrame :: EngineRecord -> Program -> Tempo -> Number -> Number -> Effect  (List MidiEvent) 
whatDoINeedToDoThisMIDIRenderFrame er p t cycleStart cycleEnd = do
  pure Nil
  --dealWithMidiLoops er p t cycleStart cycleEnd 
-}

-- figure out wakeup time, for each wake up time figure out way/method on how to change store variables (map)
whatDoINeedToDoThisRenderFrame :: EngineRecord -> Program -> Tempo -> Number -> Number -> Effect (List SampleEvent)
whatDoINeedToDoThisRenderFrame er p t cycleStart cycleEnd = do
  maybeInitializeGlobalVars er p
  dealWithLoops er p t cycleStart cycleEnd 


---

maybeInitializeGlobalVars :: EngineRecord -> Program -> Effect Unit 
maybeInitializeGlobalVars er p = traverse_ (updateGlobalVariable er) p 

updateGlobalVariable :: EngineRecord -> Element -> Effect Unit
updateGlobalVariable er (VariableGlobal s n) = do
  varMap <- liftEffect $ read $ er.variables
  seqMap <- liftEffect $ read er.sequences 

  let resolvedNumber = resolveExpression n varMap seqMap
  let newMap = case (Map.lookup s varMap) of
                 Just x ->  compareOriginalVariable er s resolvedNumber  -- resolve expression to enable arythmatic
                 Nothing -> fillMaps er s resolvedNumber  -- resolve expression to enable arythmatic
  --write newMap er.variables
  newMap

updateGlobalVariable er (GlobalSequence v xs) = do
  varMap <- liftEffect $ read $ er.variables
  sequenceMap <- liftEffect $ read $ er.sequences 
  let resolvedList = map (\n -> (resolveExpression n varMap sequenceMap)) xs
  let newMap = Map.insert v resolvedList sequenceMap
  write newMap er.sequences

updateGlobalVariable er (LoopElement _) = pure unit
--- 

fillMaps :: EngineRecord -> String -> Number -> Effect Unit
fillMaps er s n = do 
  varMap <- liftEffect $ read $ er.variables
  origMap <- liftEffect $ read $ er.originalVariables
  let newMap = Map.insert s n varMap 
  let defMap = Map.insert s n origMap 
  write newMap er.variables
  write defMap er.originalVariables

compareOriginalVariable :: EngineRecord -> String -> Number -> Effect Unit
compareOriginalVariable er s n = do
    varMap <- liftEffect $ read $ er.variables
    origMap <- liftEffect $ read $ er.originalVariables
    let originalValue = case (Map.lookup s origMap) of
                          Just x -> x
                          Nothing -> 2012.0 -- random number! it will always be there. 
    if n == originalValue then do 
      write varMap er.variables
    else do
      let defMap = Map.insert s n origMap
      let newMap = Map.insert s n varMap
      write newMap er.variables
      write defMap er.originalVariables
      log $ (show defMap)

-- if def number is same as original def, do not overwrite. if it is different. overwrite

dealWithLoops :: EngineRecord -> Program -> Tempo -> Number -> Number -> Effect (List SampleEvent)
dealWithLoops er p t wStart wEnd = do 
    -- for each loop statement, figure out when they each activate. 
        -- collect all loops. Program -> (List Loop)
        varMap <- liftEffect $ read er.variables 
        seqMap <- liftEffect $ read er.sequences 

        let ls = collectLoops p -- ::  (List Loop)
        let looptimes = concat $ map (getLoopTimes er wStart wEnd varMap seqMap) ls -- :: List (Tuple Loop Number)

        let sortedList = sortBy (comparing snd) looptimes -- :: List (Tuple Number Loop)
        events <- traverse (performLoop er) sortedList
        pure $ concat events
      -- calculate effect of all activations 

-- traverse keeps things in m context!
-- traverse_ :: unit

collectLoops :: Program -> List Loop
collectLoops p = concat $ map loopList p 

loopList :: Element -> List Loop
loopList (LoopElement x) = x:Nil
loopList (VariableGlobal _ _) = Nil 
loopList (GlobalSequence _ _) = Nil 

getLoopTimes :: EngineRecord -> Number -> Number -> Map.Map String Number  -> Map.Map String (List Number)  -> Loop -> List (Tuple Loop Number) 
getLoopTimes er wStart wEnd varMap seqMap (Loop n la)  = 
  -- calculate cycle time of occurences between wstart and wend. 
  -- for each number , produce a tuple 

    let l = (Loop n la)
        newN = resolveExpression n varMap seqMap-- converts loops expression to number 
        safeN = limitLoopTime newN   
        xs = cycleIntervalList wStart wEnd safeN -- List Number
        in map (Tuple l) xs

limitLoopTime :: Number -> Number
limitLoopTime n = do
  if n <= 0.001 then do 
    0.001
  else
    n

cycleIntervalList :: Number -> Number -> Number -> List Number
cycleIntervalList wStart wEnd n = 
  let pFirst = (toNumber $ ceil (wStart/n)) * n
  in if pFirst < wEnd then
    (pFirst : cycleIntervalList (pFirst+n) wEnd n )
  else
    Nil 

 --  for future me.... there shouldn't be two loops. This will lead to timing and sync issues. 
 --  it may be best for perform loop to return either Effect List SampleEvent or MidiEvent.

performLoop :: EngineRecord -> Tuple Loop Number -> Effect (List SampleEvent)
performLoop er (Tuple (Loop _ la) n) = performListAction er la n

performListAction :: EngineRecord -> List Action -> Number -> Effect (List SampleEvent)
performListAction er la nowCycles = do 
  xs <- traverse (performAction er nowCycles) la 
  pure $ concat xs 

 -- Either (List sampleEvent) List midiEvent
performAction :: EngineRecord -> Number -> Action -> Effect (List SampleEvent)
performAction er nowCycles (Play i x params) = do
  t <- read $ er.tempo
  sampleEvent <- performPlay er t nowCycles i x params
  pure sampleEvent

-- remember x for midiPlay is the channel#
performAction er nowCycles (MidiPlay x params) = do
  t <- read $ er.tempo
  let i = "midimidmidi"
  midiEvent <- performMidiPlay er t nowCycles i x params
  pure midiEvent

performAction er nowCycles (VariableAction x) = performVariableA er x 

performAction er nowCycles (RandomN v x xs) = do
 varMap <- liftEffect $ read $ er.variables
 seqMap <- liftEffect $ read er.sequences 

 let rX =  resolveExpression x varMap seqMap
 let rXS = resolveExpression xs varMap seqMap
 randInt <- randomInt (round rX) (round rXS)
 let randNum = toNumber randInt
 let newmap = Map.insert v randNum varMap
 write newmap er.variables
 pure Nil

performAction er nowCycles (RandomList v xs) = do
  varMap <- liftEffect $ read $ er.variables
  seqMap <- liftEffect $ read er.sequences 

  let resolvedList = map (\n -> (resolveExpression n varMap seqMap)) xs
  let listLength = length resolvedList
  randInt <- randomInt 0 listLength
  let randLookUp = case (index resolvedList randInt) of

                   Just x -> do 
                             let newmap = Map.insert v x varMap
                             write newmap er.variables
                             pure Nil

                   Nothing -> pure Nil

  randLookUp

performAction er nowCycles (Sequence v xs) = do 
  varMap <- liftEffect $ read $ er.variables
  sequenceMap <- liftEffect $ read $ er.sequences  
  let resolvedList = map (\n -> (resolveExpression n varMap sequenceMap)) xs
  let newmap = Map.insert v resolvedList sequenceMap
  write newmap er.sequences
  pure Nil

performAction er nowCycles (Conditional v comp xs ca) = do 
  varMap <- liftEffect $ read $ er.variables
  seqMap <- liftEffect $ read $ er.sequences  
  let rV =  resolveExpression v varMap seqMap -- first variable
  let rXS = resolveExpression xs varMap seqMap -- variable being compared to 

  let condBool = case comp of

                    "==" -> rV == rXS 
                    "!=" -> rV /= rXS 
                    ">" ->  rV >  rXS 
                    "<" ->  rV <  rXS 
                    ">=" -> rV >= rXS 
                    "<=" -> rV <= rXS 
                    _ -> false

  if condBool == true then do
    performListAction er ca nowCycles
    -- condition funcitons would go here. prolly case of..... < > == != <= >= ....
  else do 
    pure Nil

-- update these variables at specific times
performVariableA :: EngineRecord -> VariableA -> Effect (List SampleEvent)
performVariableA er (VariableA s e) = do 
 varMap <- liftEffect $ read $ er.variables
 sequenceMap <- liftEffect $ read $ er.sequences  
 let newmap = crunchVar varMap sequenceMap (VariableA s e)
 write newmap er.variables
 pure Nil


{- MIDI STUFF :)

dealWithMidiLoops :: EngineRecord -> Program -> Tempo -> Number -> Number -> Effect (List MidiEvent)
dealWithMidiLoops er p t wStart wEnd = do
    -- for each loop statement, figure out when they each activate. 
        -- collect all loops. Program -> (List Loop)
        varMap <- liftEffect $ read er.variables 
        seqMap <- liftEffect $ read er.sequences 

        let ls = collectLoops p -- ::  (List Loop)
        let looptimes = concat $ map (getLoopTimes er wStart wEnd varMap seqMap) ls -- :: List (Tuple Loop Number)

        let sortedList = sortBy (comparing snd) looptimes -- :: List (Tuple Number Loop)
        events <- traverse (performMidiLoop er) sortedList
        pure $ concat events
      -- calculate effect of all activations 

performMidiLoop :: EngineRecord -> Tuple Loop Number -> Effect (List MidiEvent)
performMidiLoop er (Tuple (Loop _ la) n) = performMidiListAction er la n

performMidiListAction :: EngineRecord -> List Action -> Number -> Effect (List MidiEvent)
performMidiListAction er la nowCycles = do 
  xs <- traverse (performMidiAction er nowCycles) la 
  pure $ concat xs 

performMidiAction :: EngineRecord -> Number -> Action -> Effect (List MidiEvent)
performMidiAction er nowCycles (MidiPlay x params) = do
  t <- read $ er.tempo
  midiEvent <- performMidi er t nowCycles x params
  pure midiEvent

  performMidiAction er nowCycles _ = do
  pure Nil
  
performMidiAction er nowCycles (Conditional v comp xs ca) = do 
  varMap <- liftEffect $ read $ er.variables
  seqMap <- liftEffect $ read $ er.sequences  
  let rV =  resolveExpression v varMap seqMap -- first variable
  let rXS = resolveExpression xs varMap seqMap -- variable being compared to 

  let condBool = case comp of

                    "==" -> rV == rXS 
                    "!=" -> rV /= rXS 
                    ">" ->  rV >  rXS 
                    "<" ->  rV <  rXS 
                    ">=" -> rV >= rXS 
                    "<=" -> rV <= rXS 
                    _ -> false

  if condBool == true then do
    performMidiListAction er ca nowCycles
    -- condition funcitons would go here. prolly case of..... < > == != <= >= ....
  else do 
    pure Nil
-}

crunchVar :: Map.Map String Number -> Map.Map String (List Number) -> VariableA -> Map.Map String Number 
crunchVar m mxs (VariableA s e) = Map.insert s (resolveExpression e m mxs) m -- v will be the outcome of e

performPlay :: EngineRecord -> Tempo -> Number -> String -> NumExpression -> PlayParams -> Effect (List SampleEvent)
performPlay er t nowCycles sample i params = do
  varMap <- liftEffect $ read $ er.variables
  sequenceMap <- liftEffect $ read $ er.sequences  

  let channel = 0.0 -- MIDI channel 
  let v = 0.0 -- MIDI velocity 
  let d = 0.0

  let sampleNum = resolveExpression i varMap sequenceMap -- sample #
  let n = resolveExpression params.note varMap sequenceMap -- note
  let g = resolveExpression params.gain varMap sequenceMap -- gain
  let p = resolveExpression params.pan varMap sequenceMap -- pitch
  let c = round (resolveExpression params.cut varMap sequenceMap) -- cut
  let se = sampleNaming t sample nowCycles sampleNum n g p c channel v d-- put it all together
  pure $ pure se

performMidiPlay :: EngineRecord -> Tempo -> Number -> String -> NumExpression -> MidiParams -> Effect (List SampleEvent)
performMidiPlay er t nowCycles sample i params = do
  varMap <- liftEffect $ read $ er.variables
  sequenceMap <- liftEffect $ read $ er.sequences 

  let channel = resolveExpression i varMap sequenceMap -- MIDI channel 
  let v = resolveExpression params.velocity varMap sequenceMap -- MIDI velocity
  let d = resolveExpression params.duration varMap sequenceMap
  let sampleNum = 0.0 
  let n = resolveExpression params.note varMap sequenceMap -- note
  let g = 0.0 -- gain
  let p = 0.0 -- pitch
  let c = 0 -- cut
  let se = sampleNaming t sample nowCycles sampleNum n g p c channel v d-- put it all together
  pure $ pure se

performMidi :: EngineRecord -> Tempo -> Number ->  NumExpression -> MidiParams -> Effect (List MidiEvent)
performMidi er t nowCycles channel params = do
  varMap <- liftEffect $ read $ er.variables
  sequenceMap <- liftEffect $ read $ er.sequences  

  let midiChannel = resolveExpression channel varMap sequenceMap 
  let n = resolveExpression params.note varMap sequenceMap -- note
  let d = resolveExpression params.duration varMap sequenceMap -- duration
  let v = resolveExpression params.velocity varMap sequenceMap -- gain
  let me = midiEventHandler t midiChannel nowCycles n v d
  pure $ pure me


resolveExpression :: NumExpression -> Map.Map String Number -> Map.Map String (List Number) -> Number 
resolveExpression (Addition x xs) m mxs  = resolveExpression x m mxs + resolveExpression xs m mxs
resolveExpression (Subtraction x xs) m mxs  = resolveExpression x m mxs - resolveExpression xs m mxs
resolveExpression (Division x xs) m mxs = resolveExpression x m mxs / resolveExpression xs m mxs
resolveExpression (Multiplication x xs) m  mxs = resolveExpression x m mxs * resolveExpression xs m mxs
resolveExpression (SequenceRead xs i) m mxs = readSequenceNumber (resolveExpression i m mxs) xs mxs -- xs is List, i is index
resolveExpression (VariableRead x ) m mxs= varRead x m 
resolveExpression (Constant x ) m mxs = x

-- find numbered index in List. index:list
readSequenceNumber :: Number -> String -> Map.Map String (List Number) -> Number 
readSequenceNumber i xs mxs = do
  let list  = case (Map.lookup xs mxs) of -- looks to see if list is real

                 Just listExists -> do 
                      let number = case (index listExists (round i)) of
            
                                  Just positionExists  -> positionExists

                                  Nothing -> i -- this is confusing and sounds like garbage lol

                      number              
                 Nothing -> i

  list


varRead :: String -> Map.Map String Number -> Number
varRead x m= do
  let varOut = case (Map.lookup x m) of
                 Just i -> i
                 Nothing -> 0.0
  varOut


--- NEED TO CHANGE THIS TO ADD FUNCTIONS
play :: WebDirt -> List SampleEvent -> Effect Unit
play wd es = for_ es \i -> do
  let gainFixed = gainFix (i.gain/100.0) -- webdirt requires that sounds range from 0-2. 1 is normal volume
  let panFixed = panFix (i.pan/100.0) -- 0 left 100 right
  if i.note >= 50.0 then do
    let rNote = 50.0
    playSample wd {s: i.s, n: i.n, whenPosix: i.whenPosix, gain: gainFixed, note:rNote, pan:panFixed, cut:i.cut }
    pure unit
    else do 
    if i.note <= -50.0 then do
      let rNote = -50.0
      playSample wd {s: i.s, n: i.n, whenPosix: i.whenPosix, gain: gainFixed, note:rNote, pan:panFixed , cut:i.cut}
      pure unit
    else do 
      playSample wd {s: i.s, n: i.n, whenPosix: i.whenPosix, gain: gainFixed, note:i.note, pan:panFixed , cut:i.cut}
      pure unit

gainFix :: Number -> Number
gainFix g = do
  if g >= 1.0 then do
    1.0
    else do 
    if g <= 0.0 then do
      0.0
    else do 
      g

panFix :: Number -> Number
panFix p = do
  if p >= 1.0 then do
    1.0
    else do 
    if p <= 0.0 then do
      0.0
    else do 
      p

printToConsole :: List SampleEvent -> Effect Unit
printToConsole es = log (show es)