module Compile where

import Parse

import Data.List (intercalate)

mkTree = parse

data Pieces = Piece String | Pieces [Pieces]

instance Show Pieces where
  show (Piece x) = x
  show (Pieces xs) = "(" ++ (intercalate " " $ map show xs) ++ ")"

chunks' :: String -> Maybe [Either String LispData]
chunks' str = fmap (map mkTree) $ mkList str

chunks :: String -> Either String [LispData]
chunks str = case chunks' str of
  Nothing -> Left "Could not parse string into lists"
  Just strs -> foldr (\a b -> return (:) <*> a <*> b) (Right []) strs

primsMap :: [(String, Primitive)]
primsMap = map (\p -> (show p, p)) primList

getPrim :: String -> Maybe Primitive
getPrim str = case filter (\(x, _) -> x == str) primsMap of
  (_, y):_ -> Just y
  _ -> Nothing

haskellPrimitive :: Primitive -> Pieces
haskellPrimitive p = Pieces $ (Piece "LispLambda"):[Piece $ getName p]
  where getName p = case p of
          LispAdd -> "add"
          LispMult -> "mult"
          LispNegate -> "negate"
          LispInvert -> "invert"
          LispCons -> "cons"
          LispCdr -> "cdr"
          LispCar -> "car"
          LispEq -> "eq"
          LispIf -> "if'"

showPiecesList :: [Pieces] -> String
showPiecesList pieces = "[" ++ (intercalate "," $ map show pieces) ++ "]"

toHaskell :: LispData -> Pieces
toHaskell ld = case ld of
  LispInt x -> Piece $ "LispInt " ++ show x
  LispPair x y -> Pieces [Piece "lispCons", toHaskell x, toHaskell y]
  LispUnit -> Piece "LispUnit"
  LispString x -> Piece $ "LispString " ++ show x
  LispSymbol x -> Piece $ "LispSymbol " ++ show x
  LispList [] -> toHaskell LispUnit
  LispList [(LispVar "print"), x] -> Pieces $ [Piece "print $", toHaskell x]
  LispList (x:xs) -> Pieces $ [Piece "apply", toHaskell x, Piece $ showPiecesList $ map toHaskell xs]
  LispFloat x -> Piece $ "LispFloat " ++ show x
  LispFraction x y -> Piece $ "LispFraction " ++ show x ++ " " ++ show y
  LispLambda [] body -> Pieces [Piece "LispLambda $", Piece "\\_ -> ", toHaskell body]
  LispLambda args body -> Pieces [Piece "LispLambda $", Piece $ "\\[" ++ (intercalate "," args) ++ "] -> ", toHaskell body]
  LispDef var body -> Piece $ "let " ++ var ++ " = " ++ show (toHaskell body)
  LispVar var -> case getPrim var of
    Just prim -> toHaskell $ LispPrimitive prim
    Nothing -> Piece var
  LispError _ -> Piece ""
  LispPrimitive p -> haskellPrimitive p
  LispBool x -> Piece $ "LispBool " ++ show x

varCheck :: [LispData] -> Bool
varCheck = varCheck' ("mkList":"print":map show primList)
  where
    varCheck' _ [] = True
    varCheck' vars (x:xs) = case x of
      LispVar vn -> if vn `elem` vars then varCheck' vars xs else False
      LispDef vn _ -> varCheck' (vn:vars) xs
      _ -> varCheck' vars xs

compileToString :: [LispData] -> Maybe String
compileToString lds = if varCheck lds
  then Just $ intercalate "\n  " $ map (show . toHaskell) lds
  else Nothing

mkHaskell :: [LispData] -> Maybe String
mkHaskell ld = do
  innerStr <- compileToString ld
  let result = "import Initial\nmain :: IO ()\nmain = do\n  " ++ innerStr
  return result

compile :: String -> Either String String
compile str = do
  lst <- chunks str
  case mkHaskell lst of
        Just result -> return result
        Nothing -> Left "Error: undefined variable"
