module Parse where

import Strings
import Regex
import Data.List (isPrefixOf, intercalate)

data Primitive = LispAdd | LispMult | LispNegate | LispInvert | LispCons | LispCar | LispCdr | LispEq | LispIf
  deriving Eq

primList :: [Primitive]
primList = [
  LispAdd,
  LispMult,
  LispNegate,
  LispInvert,
  LispCons,
  LispCar,
  LispCdr,
  LispEq,
  LispIf
           ]

instance Show Primitive where
  show LispAdd = "+"
  show LispMult = "*"
  show LispNegate = "negate"
  show LispInvert = "invert"
  show LispCons = "cons"
  show LispCar = "car"
  show LispCdr = "cdr"
  show LispEq = "="
  show LispIf = "if"

data LispData =
  LispInt Integer
  | LispPair LispData LispData
  | LispUnit
  | LispString String
  | LispSymbol String
  | LispList [LispData]
  | LispFloat Double
  | LispFraction Integer Integer
  | LispLambda [String] LispData
  | LispDef String LispData
  | LispVar String
  | LispError String
  | LispPrimitive Primitive
  | LispBool Bool
  deriving Eq

instance Show LispData where
  show ld = case ld of
    LispInt x -> show x
    LispString x -> '"':x ++ "\""
    LispSymbol x -> x
    LispList xs -> '(':(intercalate " " $ map show xs) ++ ")"
    LispFloat x -> show x
    LispFraction x y -> show x ++ "/" ++ show y
    LispLambda xs f -> "(lambda (" ++ (intercalate " " xs) ++ ") " ++ show f ++ ")"
    LispDef x y -> "(def " ++ x ++ " " ++ show y ++ ")"
    LispVar x -> x
    LispError x -> "ERROR: " ++ x
    LispUnit -> "'()"
    LispPair x y -> "(" ++ show x ++ " . " ++ show y ++ ")"
    LispBool True -> "#t"
    LispBool False -> "#f"
    LispPrimitive p -> "Primitive " ++ show p

digits = ['0'..'9']
lowercase = ['a'..'z']
uppercase = ['A'..'Z']
alpha = lowercase ++ uppercase
alphanumeric = digits ++ alpha
everything = [' '..'~']

type LispParser = String -> Maybe LispData

lispUnit :: LispParser
lispUnit str = regex [Start, Characters "'" 1, Characters "(" 1, Characters ")" 1, End] str >> return LispUnit

lispBool :: LispParser
lispBool str = do
  matched <- regex [Start, Characters "#" 1, Characters "tf" 1, End] str
  let val = case (last matched) of
        't' -> True
        'f' -> False
  return $ LispBool val

lispInt :: LispParser
lispInt str = do
  matched <- regex [Start, SomeCharacters digits, End] str
  let intVal = (read matched) :: Integer
  return $ LispInt intVal

lispString :: LispParser
lispString str = do
  string <- regex [Start, Characters "\"" 1, MaybeNoneOf "\"", Characters "\"" 1, End] str
  return $ LispString $ init $ tail $ string

lispSymbol :: LispParser
lispSymbol str = do
  matched <- regex [Start, Characters "'" 1, SomeNoneOf " ", End] str
  return $ LispSymbol $ tail matched

lispList :: LispParser
lispList str = do
  matched <- regex [Start, SomeCharacters "(", MaybeCharacters everything, SomeCharacters ")", End] str
  strings <- mkList matched
  let parsed = map parse strings
  if filter isLeft parsed /= []
    then Nothing
    else return $ LispList $ map unEither parsed
         where unEither (Right x) = x

lispFloat :: LispParser
lispFloat str = do
  matched <- regex [Start, MaybeCharacters digits, MaybeCharacter ".", MaybeCharacters digits, End] str
  let floatVal = (read matched) :: Double
  return $ LispFloat floatVal

lispFraction :: LispParser
lispFraction str = do
  matched <- regex [Start, SomeCharacters digits, Characters "/" 1, SomeCharacters digits, End] str
  let separate = split '/' matched
  let numer = (read $ head separate) :: Integer
  let denom = (read $ last separate) :: Integer
  return $ LispFraction numer denom

lispLambda :: LispParser
lispLambda str = do
  items <- mkList str
  if (length items == 3) && (items !! 0 == "lambda")
    then do
      varList <- mkList $ items !! 1
      expr <- unEither $ parse $ items !! 2
      return $ LispLambda varList expr
    else Nothing

lispDef str = do
  items <- mkList str
  if (length items >= 3) && (items !! 0 == "def")
    then do
      expr <- unEither $ parse $ concat $ tail $ tail items
      return $ LispDef (items !! 1) expr
    else Nothing

lispVar :: LispParser
lispVar str = do
  matched <- regex [Start, NoneOf "'" 1, SomeNoneOf " ", End] str
  return $ LispVar matched

parsers :: [LispParser]
-- Order is significant!  They get checked for in the order that they are in the list.
parsers = [
  lispDef,
  lispLambda,
  lispUnit,
  lispSymbol,
  lispInt,
  lispString,
  lispFloat,
  lispFraction,
  lispList,
  lispBool,
  lispVar
          ]

mkList :: String -> Maybe [String]
mkList str = do
  all <- inBetween '(' ')' str
  items <- groupBy (inBetweenDual '(' ')') (init $ tail all) []
  let separated = map (\val -> case val of
                          Left str -> split ' ' str
                          Right str -> [str]) items
  return $ filter (/= []) $ concat separated

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

unEither :: Either a b -> Maybe b
unEither (Right x) = Just x
unEither _ = Nothing

runParsers :: [LispParser] -> String -> Maybe LispData
runParsers [] _ = Nothing
runParsers (p:ps) str = case p str of
  Nothing -> runParsers ps str
  Just ld -> Just ld

parse :: String -> Either String LispData
parse str = case runParsers parsers str of
  Nothing -> Left $ "Failed to parse string: " ++ str
  Just ld -> Right ld
