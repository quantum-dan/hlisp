module Parse where

import Strings

data LispData =
  LispInt Integer
  | LispString String
  | LispSymbol String
  | LispList [LispData]
  | LispFloat Double
  | LispFraction Integer Integer
  | LispLambda (LispData -> LispData)

mkList :: String -> Maybe [String]
mkList str = do
  all <- inBetween '(' ')' str
  initial <- Strings.until '(' all
  rest <- groupBy (inBetweenRem '(' ')') all
  return $ filter (\x -> x /= []) $ (split ' ' initial) ++ rest
