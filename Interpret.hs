module Interpret where

import Parse

type PF = LispData -> LispData
lispAdd' :: LispData -> LispData -> LispData
lispAdd' (LispInt x) (LispInt y) = LispInt $ x + y
lispAdd' (LispFloat x) (LispFloat y) = LispFloat $ x + y
lispAdd' (LispInt x) (LispFloat y) = LispFloat $ (fromIntegral x) + y
lispAdd' (LispFloat x) (LispInt y) = LispFloat $ x + (fromIntegral y)
lispAdd' _ _ = LispError "Error: can't add non-numeric values"

lispAdd :: [LispData] -> LispData
lispAdd = foldl lispAdd' (LispInt 0)

lispNegate :: PF
lispNegate (LispInt x) = LispInt (-x)
lispNegate (LispFloat x) = LispFloat (-x)
lispNegate _ = LispError "Error: can't negate non-numeric values"

lispMult' :: LispData -> LispData -> LispData
lispMult' (LispInt x) (LispInt y) = LispInt $ x * y
lispMult' (LispFloat x) (LispFloat y) = LispFloat $ x * y
lispMult' (LispInt x) (LispFloat y) = LispFloat $ (fromIntegral x) * y
lispMult' (LispFloat x) (LispInt y) = LispFloat $ x * (fromIntegral y)
lispMult' _ _ = LispError "Error: can't multiply non-numeric values"

lispMult :: [LispData] -> LispData
lispMult = foldl lispMult' (LispInt 1)

lispInvert :: PF
lispInvert (LispFloat x) = LispFloat $ 1 / x
lispInvert _ = LispError "Error: can't invert non-floating point values"

lispCons :: [LispData] -> LispData
lispCons items@(x:xs)
  | length items == 2 = LispPair (head items) (last items)
  | otherwise = foldr LispPair LispUnit items

lispCar :: PF
lispCar (LispPair a b) = a
lispCar _ = LispError "Error: car must be applied to a pair"

lispCdr :: PF
lispCdr (LispPair a b) = b
lispCdr _ = LispError "Error: cdr must be applied to a pair"

primitives :: [Var]
primitives = map (\x -> Var (show x) (LispPrimitive x)) primList

getMultiArgsPrim :: Primitive -> ([LispData] -> LispData)
getMultiArgsPrim LispAdd = lispAdd
getMultiArgsPrim LispMult = lispMult
getMultiArgsPrim LispCons = lispCons

getUnaryPrim :: Primitive -> (LispData -> LispData)
getUnaryPrim LispNegate = lispNegate
getUnaryPrim LispInvert = lispInvert
getUnaryPrim LispCar = lispCar
getUnaryPrim LispCdr = lispCdr

applyPrimitive :: Primitive -> [LispData] -> LispData
applyPrimitive primitive args
  | primitive `elem` [LispAdd, LispMult, LispCons] = getMultiArgsPrim primitive args
  | primitive `elem` [LispNegate, LispInvert, LispCar, LispCdr] && length args == 1 = getUnaryPrim primitive $ head args
  | otherwise = LispError $ "Wrong number of arguments to function " ++ show primitive ++ " or implementation bug"

data Var = Var {
  varName :: String,
  varData :: LispData
               }
  deriving (Show, Eq)
type Env = [Var]

data Environment a = Environment {
  envExpr :: LispData,
  envData :: a
  }
  deriving Show

instance Functor Environment where
  fmap f (Environment ld env) = Environment ld (f env)
instance Applicative Environment where
  pure = Environment (LispInt 43)
  (Environment ld f) <*> (Environment _ env) = Environment ld $ f env

instance Monad Environment where
  return = pure
  (Environment ld env) >>= f = f env

infixl ==>
(==>) :: Environment Env -> (LispData -> Env -> Environment Env) -> Environment Env
(Environment ld env) ==> f = f ld env

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

getVar :: Env -> LispData -> LispData
getVar env (LispVar vn) = case fmap varData $ safeHead $ filter (\v -> varName v == vn) env of
  Nothing -> LispError $ "Error: undefined variable: " ++ vn
  Just ld -> ld
getVar _ _ = LispError "Error: interpreter bug caused attempted retrieval of a non-variable"

addVar :: Env -> LispData -> Env
addVar env (LispDef name contents) = (Var name contents):env
addVar env _ = env

interpret :: LispData -> Env -> Environment Env
interpret lv@(LispVar _) env = Environment (getVar env lv) env
interpret (LispList items) env = Environment (LispList $ map (envExpr . (\x -> interpret x env)) items) env
  ==> \(LispList items) env -> apply (head items) (tail items) env
interpret ld@(LispDef name expr) env = let (Environment expr' _) = interpret expr env in Environment ld $ addVar env $ LispDef name expr'
interpret ld env = Environment ld env

run :: String -> Env -> Environment Env
run str env = case parse str of
  Left err -> Environment (LispError err) []
  Right ld -> interpret ld env

apply :: LispData -> [LispData] -> Env -> Environment Env
apply (LispPrimitive primitive) args env =
  let args' = map (\a -> envExpr $ interpret a env) args
      result = applyPrimitive primitive args'
  in Environment result env
apply (LispLambda argList body) args env =
  if length argList == length args
     then let envArgs = zipWith Var argList args ++ env
          in interpret body envArgs
  else Environment (LispError "Error: too many or too few arguments passed to function") []
apply _ _ _ = Environment (LispError "Error: cannot apply non-function") []

testFn :: Environment Env
testFn = return []
  >>= run "(def fn (lambda () 5))"
  >>= run "(fn)"
  >>= run "(def fn2 (lambda (x) x))"
  >>= run "(fn2 fn)"
  >>= run "((fn2 fn))"

mkList args = LispList args
testList = [LispInt 1, LispSymbol "hello"]

testFn' :: Environment Env
testFn' = return primitives
  >>= run "(def var 'hello)"
  >>= run "(cons 1 var \"fish\")"
  >>= run "(* 1.0 2 3 (negate 4) (invert 3.0))"
