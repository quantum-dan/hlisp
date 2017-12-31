module Parse where

import Strings
import Regex
import Data.List (isPrefixOf)

data LispData =
  LispInt Integer
  | LispString String
  | LispSymbol String
  | LispList [LispData]
  | LispFloat Double
  | LispFraction Integer Integer
  | LispLambda (LispData -> LispData)
  | LispDef String LispData
  | LispVar String

instance Show LispData where
  show (LispInt x) = "LispInt " ++ show x
  show (LispString x) = "LispString " ++ x
  show (LispSymbol x) = "LispSymbol " ++ x
  show (LispList x) = "LispList " ++ show x
  show (LispFloat x) = "LispFloat " ++ show x
  show (LispFraction x y) = "LispFraction " ++ show x ++ "/" ++ show y
  show (LispLambda _) = "LispLambda"
  show (LispDef x y) = "LispDef " ++ x ++ " = " ++ show y
  show (LispVar x) = "LispVar " ++ x

instance Eq LispData where
  LispInt x == LispInt y = x == y
  LispString x == LispString y = x == y
  LispSymbol x == LispSymbol y = x == y
  LispList x == LispList y = x == y
  LispFloat x == LispFloat y = x == y
  LispFraction x1 x2 == LispFraction y1 y2 = (quot x1 x2) == (quot y1 y2)
  LispVar x == LispVar y = x == y
  _ == _ = False

digits = ['0'..'9']
lowercase = ['a'..'z']
uppercase = ['A'..'Z']
alpha = lowercase ++ uppercase
alphanumeric = digits ++ alpha
everything = [' '..'~']

type LispParser = String -> Maybe LispData

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
  matched <- regex [Start, Characters "(" 1, MaybeCharacters everything, Characters ")" 1, End] str
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
lispLambda str
  | length str > length "lambda" && isPrefixOf "lambda" str = return $ LispLambda (\x -> LispInt 43) -- not the answer
  | otherwise = Nothing

lispDef str
  | length str > length "def" && isPrefixOf "def" str = do
      let items = split ' ' str
      if length items >= 3
        then do
          expr <- unEither $ parse $ concat (tail $ tail items) -- after the first 2
          return $ LispDef (items !! 1) expr
        else Nothing
  | otherwise = Nothing

lispVar :: LispParser
lispVar str = do
  matched <- regex [Start, NoneOf "'" 1, SomeNoneOf " ", End] str
  return $ LispVar matched

parsers :: [LispParser]
-- Order is significant!
parsers = [
  lispDef,
  lispLambda,
  lispSymbol,
  lispInt,
  lispString,
  lispFloat,
  lispFraction,
  lispList,
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
