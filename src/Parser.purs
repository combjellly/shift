module Parser where

import Prelude

import Parsing
import Parsing.Language (emptyDef)
import Parsing.Token (GenLanguageDef(..),LanguageDef,unGenLanguageDef,TokenParser,GenTokenParser,makeTokenParser)
import Parsing.Combinators
import Data.List.NonEmpty
import Data.Identity
import Effect.Console
import Data.Int (toNumber)
import Data.Array (many)
import Data.List
import Data.Either

import AST


type P a = ParserT String Identity a

-- Dictionary
tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser $ LanguageDef (unGenLanguageDef emptyDef) {
  reservedNames = ["every","play","if","elif","else","random"],
  reservedOpNames = [",","=","+","-","*","/","[","]",":","==","!=",">","<",">=","<="]
  }

parsetest :: String -> Either ParseError (List Element)
parsetest x = do
    runParser x program

program :: P Program
program = sepBy elementChoice (whiteSpace)

elementChoice :: P Element
elementChoice = choice [
      try $ loopElement,
      try $ globalSequence,
      try $ variableGlobal
      ]

loopElement :: P Element
loopElement = do
  l <- loop
  pure $ LoopElement l

loop :: P Loop
loop = do
  reserved "every"
  n <- variableTask
  xs <- listOfActions
  pure $ Loop n xs


-- VariableGlobal

globalSequence :: P Element 
globalSequence = do
  v <- identifier
  reservedOp "="
  reservedOp "["
  xs <- listOfNum
  reservedOp "]"
  pure $ GlobalSequence v xs

variableGlobal :: P Element
variableGlobal = do
  x <- identifier
  reservedOp "="
  xs <- variableTask -- detNum -- converting int to Number. perhaps variable task. Make it String Numexpression
  pure $ VariableGlobal x xs

-----------------------------

listOfActions :: P (List Action)
listOfActions = sepBy action (whiteSpace)
-- not going to have spaces NumExpressions y=6*8+2.... 

-- maybe implement try here. 
action :: P Action
action = choice [
    try $ conditional,
    try $ sequence,
    try $ randomList,
    try $ genRandNum,
    try $ variableAction,
    try $ playActionCut, 
    try $ playActionPan, 
    try $ playActionNote, 
    try $ playActionGain,
    try $ playAction
]

listOfConditionalActions :: P (List Action)
listOfConditionalActions = sepBy conditionalAction (whiteSpace) 

conditionalAction :: P Action
conditionalAction = choice [
    try $ sequence,
    try $ randomList,
    try $ genRandNum,
    try $ variableAction,
    try $ playActionCut, 
    try $ playActionPan, 
    try $ playActionNote, 
    try $ playActionGain,
    try $ playAction
]



playAction :: P Action
playAction = do
  -- _ <- pure unit. example of anywhere!
  reserved "play"
  reservedOp "."
  x <- identifier -- :: P String
  i <- sampleNumber
  pure $ Play x i { gain : Constant 80.0, note :Constant  0.0, pan:Constant 50.0, cut: Constant 0.0}

sampleNumber :: P NumExpression
sampleNumber = choice [
          try $ withNumber,
          try $ withoutNumber
          ]

withNumber :: P NumExpression
withNumber = do
  reservedOp ":"
  n <- variableTask
  pure $ n

withoutNumber :: P NumExpression
withoutNumber = do
  pure $ Constant 0.0

playActionGain :: P Action
playActionGain = do
  reserved "play"
  reservedOp "."
  x <- identifier
  i <- sampleNumber
  reservedOp "["
  g <- variableTask
  reservedOp "]"
  pure $ Play x i { gain :g, note :Constant  0.0, pan:Constant 50.0 , cut: Constant 0.0}


playActionNote :: P Action
playActionNote = do
  reserved "play"
  reservedOp "."
  x <- identifier
  i <- sampleNumber
  reservedOp "["
  g <- variableTask
  reservedOp ","
  n <- variableTask
  reservedOp "]"
  pure $ Play x i {gain :g, note :n, pan:Constant 50.0 , cut: Constant 0.0}


playActionPan :: P Action
playActionPan = do
  reserved "play"
  reservedOp "."
  x <- identifier
  i <- sampleNumber
  reservedOp "["
  g <- variableTask
  reservedOp ","
  n <- variableTask
  reservedOp ","
  p <- variableTask
  reservedOp "]"
  pure $ Play x i {gain :g, note :n, pan:p, cut: Constant 0.0}


playActionCut :: P Action
playActionCut = do
  reserved "play"
  reservedOp "."
  x <- identifier
  i <- sampleNumber
  reservedOp "["
  g <- variableTask
  reservedOp ","
  n <- variableTask
  reservedOp ","
  p <- variableTask
  reservedOp ","
  c <- variableTask
  reservedOp "]"
  pure $ Play x i {gain :g, note :n, pan:p, cut:c}


variableAction :: P Action
variableAction = do
  v <- variableA
  pure $ VariableAction v

randomList :: P Action
randomList = do
  v <- identifier
  reservedOp "="
  reserved "random"
  reservedOp "."
  reservedOp "["
  xs <- listOfNum
  reservedOp "]"
  pure $ RandomList v xs

listOfNum :: P (List NumExpression)
listOfNum = sepBy variableTask (whiteSpace)

genRandNum :: P Action
genRandNum = do
  v <- identifier
  reservedOp "="
  reserved "random"
  reservedOp "."
  x <- variableTask
  xs <- variableTask
  pure $ RandomN v x xs


sequence :: P Action
sequence = do
  v <- identifier
  reservedOp "="
  reservedOp "["
  xs <- listOfNum
  reservedOp "]"
  pure $ Sequence v xs 


-- Comparison // Conditionals

conditional :: P Action
conditional = do
  reserved "if"  
  choice [
    try $ equalTo, -- ==
    try $ notEqualTo, -- !=
    try $ greaterThan, -- >
    try $ lessThan, -- <
    try $ greaterThanOrEqualTo, -- >=
    try $ lesserThanOrEqualTo -- <= 
    ]

equalTo :: P Action
equalTo = do
  v <- variableTask 
  reservedOp "==" 
  xs <- variableTask
  reservedOp "[" 
  ca <- listOfConditionalActions
  reservedOp "]" 
  pure $ Conditional v "==" xs ca

notEqualTo :: P Action
notEqualTo = do
  v <- variableTask 
  reservedOp "!=" 
  xs <- variableTask
  reservedOp "[" 
  ca <- listOfConditionalActions
  reservedOp "]" 
  pure $ Conditional v "!=" xs ca

greaterThan :: P Action
greaterThan = do
  v <- variableTask 
  reservedOp ">" 
  xs <- variableTask
  reservedOp "[" 
  ca <- listOfConditionalActions
  reservedOp "]" 
  pure $ Conditional v ">" xs ca

lessThan :: P Action
lessThan = do
  v <- variableTask 
  reservedOp "<" 
  xs <- variableTask
  reservedOp "[" 
  ca <- listOfConditionalActions
  reservedOp "]" 
  pure $ Conditional v "<" xs ca

greaterThanOrEqualTo :: P Action
greaterThanOrEqualTo = do
  v <- variableTask 
  reservedOp ">=" 
  xs <- variableTask
  reservedOp "[" 
  ca <- listOfConditionalActions
  reservedOp "]" 
  pure $ Conditional v ">=" xs ca

lesserThanOrEqualTo :: P Action
lesserThanOrEqualTo = do
  v <- variableTask 
  reservedOp "<=" 
  xs <- variableTask
  reservedOp "[" 
  ca <- listOfConditionalActions
  reservedOp "]" 
  pure $ Conditional v "<=" xs ca

variableA :: P VariableA
variableA = do
  v <- identifier
  reservedOp "="
  task <- variableTask
  pure $ VariableA v task

---- variable task

variableTask :: P NumExpression
variableTask = do 
  _ <- pure unit
  choice [
    try $ sequenceRead,
    try $ variableTaskArithmetic
    ]

sequenceRead :: P NumExpression
sequenceRead = do
  xs <- identifier
  reservedOp ":"
  i <- variableTask
  pure $ SequenceRead xs i

variableTaskArithmetic :: P NumExpression
variableTaskArithmetic = do
  _ <- pure unit
  chainl1 variableTask' additionSubtraction

additionSubtraction :: P (NumExpression -> NumExpression -> NumExpression)
additionSubtraction = choice [
  reservedOp "+" $> Addition,
  reservedOp "-" $> Subtraction
  ]


variableTask' :: P NumExpression
variableTask' = do 
  _ <- pure unit
  chainl1 variableTask'' divisionMultiplication


divisionMultiplication :: P (NumExpression -> NumExpression -> NumExpression)
divisionMultiplication = choice [
  reservedOp "/" $> Division, 
  reservedOp "*" $> Multiplication
  ]

variableTask'' :: P NumExpression
variableTask'' = do 
  _ <- pure unit
  choice [
    parens variableTask,
    try $ Constant <$> negativeFloat,
    try $ Constant <$> detNum,
    try $ VariableRead <$> identifier
    ]

negativeFloat :: P Number
negativeFloat = do
  reservedOp "-"
  ((*) (-1.0)) <$> detNum

naturalOrFloatToNumber :: Either Int Number -> Number
naturalOrFloatToNumber (Left i) = toNumber i
naturalOrFloatToNumber (Right n) = n

-- Type Decleration

angles :: forall a. P a -> P a
angles = tokenParser.angles

braces :: forall a. P a -> P a
braces = tokenParser.braces

brackets :: forall a. P a -> P a
brackets = tokenParser.brackets

charLiteral :: P Char
charLiteral = tokenParser.charLiteral

colon :: P String
colon = tokenParser.colon

comma :: P String
comma = tokenParser.comma

commaSep :: forall a. P a -> P (List a)
commaSep = tokenParser.commaSep

commaSep1 :: forall a. P a -> P (NonEmptyList a)
commaSep1 = tokenParser.commaSep1

decimal :: P Int
decimal = tokenParser.decimal

detNum :: P Number 
detNum = naturalOrFloatToNumber <$> tokenParser.naturalOrFloat

dot :: P String
dot = tokenParser.dot

float :: P Number
float = tokenParser.float

hexadecimal :: P Int
hexadecimal = tokenParser.hexadecimal

identifier :: P String
identifier = tokenParser.identifier

integer :: P Int
integer = tokenParser.integer

lexeme :: forall a. P a -> P a
lexeme = tokenParser.lexeme

natural :: P Int
natural = tokenParser.natural

naturalOrFloat :: P (Either Int Number)
naturalOrFloat = tokenParser.naturalOrFloat

octal :: P Int
octal = tokenParser.octal

operator :: P String
operator = tokenParser.operator

parens :: forall a. P a -> P a
parens = tokenParser.parens

reserved :: String -> P Unit
reserved = tokenParser.reserved

reservedOp :: String -> P Unit
reservedOp = tokenParser.reservedOp

semi :: P String
semi = tokenParser.semi

semiSep :: forall a. P a -> P (List a)
semiSep = tokenParser.semiSep

semiSep1 :: forall a. P a -> P (NonEmptyList a)
semiSep1 = tokenParser.semiSep1

stringLiteral :: P String
stringLiteral = tokenParser.stringLiteral

symbol :: String -> P String
symbol = tokenParser.symbol

whiteSpace :: P Unit
whiteSpace = tokenParser.whiteSpace
