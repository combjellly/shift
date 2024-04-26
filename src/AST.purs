module AST where

import Prelude
import Data.Monoid.Dual
import Data.List
import Data.Show
import Data.Either
import Data.Maybe
import Effect


type Program = (List Element)

data Element = LoopElement Loop 
                | VariableGlobal String NumExpression 
                | GlobalSequence String (List NumExpression)

data Loop = Loop NumExpression (List Action)

data Action = Play String NumExpression PlayParams 
              | MidiPlay NumExpression MidiParams
              | VariableAction VariableA 
              | RandomN String NumExpression NumExpression 
              | RandomList String (List NumExpression)
              | Sequence String (List NumExpression)
              | Conditional  NumExpression String NumExpression (List Action)

-- Should Gain come before Note in parse? What may the priority be)
type PlayParams = {gain :: NumExpression, note :: NumExpression, pan :: NumExpression, cut :: NumExpression}
type MidiParams = {note :: NumExpression, velocity :: NumExpression, duration :: NumExpression}


data VariableA = VariableA String NumExpression

data NumExpression =  
        Addition NumExpression NumExpression |
        Subtraction NumExpression NumExpression|
        Division NumExpression NumExpression|
        Multiplication NumExpression NumExpression|
        SequenceRead String NumExpression|
        VariableRead String | -- variable name being referenced 
        Constant Number 
 -- number

instance showElement :: Show Element where
    show (LoopElement x) = "[" <>show x<> "]"
    show (VariableGlobal x xs) = "[" <>show x <> "=" <> show xs <> "]"
    show (GlobalSequence x xs) = "[" <>show x <> "=" <> show xs <> "]"


instance showLoop :: Show Loop where
    show (Loop n xs) = "(Every " <> show n <> " " <> show xs <> ")"

instance showAction :: Show Action where
    show (Play x n i) = "play.(" <> show x <> ") :"<> show n <> "[" <> show i <> "]"
    show (MidiPlay x i) = "midi:"<> show x <> "[" <> show i <> "]"
    show (VariableAction x) = "(" <>show x<> ")"
    show (RandomN v x xs) = "rand." <> show x <> "," <> show xs 
    show (RandomList v xs) = "rand.[" <> show xs <> "]"
    show (Sequence v xs) = show v <> "[ " <> show xs <> "]"
    show (Conditional v comp xs ca) = "if" <> show v <> show comp <> show xs <> "do " <> show ca


instance showVariableA :: Show VariableA where
    show (VariableA x xs) = show x <> "=" <> show xs

instance showNumExpression :: Show NumExpression where
    show (Addition x xs) = "[" <>show x <> "+" <> show xs <> "]"
    show (Subtraction x xs) = "[" <>show x <> "-" <> show xs <> "]"
    show (Division x xs) = "[" <>show x <> "/" <> show xs <> "]"
    show (Multiplication x xs) = "[" <> show x <> "*" <> show xs <> "]"
    show (SequenceRead xs i) = "[" <> show xs <> ":" <> show i <> "]"
    show (VariableRead x) = show x
    show (Constant x) = show x

-- show instance later