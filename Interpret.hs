module Interpret where

import Parse

type PF = LispData -> LispData
lispAdd :: LispData -> LispData -> LispData
lispAdd (LispInt x) (LispInt y) = LispInt $ x + y
lispAdd (LispFloat x) (LispFloat y) = LispFloat $ x + y
lispAdd _ _ = LispError "Error: can't add non-numeric values"

lispNegate :: PF
lispNegate (LispInt x) = LispInt (-x)
lispNegate (LispFloat x) = LispFloat (-x)
lispNegate _ = LispError "Error: can't negate non-numeric values"

lispMult :: LispData -> LispData -> LispData
lispMult (LispInt x) (LispInt y) = LispInt $ x * y
lispMult (LispFloat x) (LispFloat y) = LispFloat $ x * y
lispMult _ _ = LispError "Error: can't multiply non-numeric values"

lispInvert :: PF
lispInvert (LispFloat x) = LispFloat $ 1 / x
lispInvert _ = LispError "Error: can't invert non-floating point values"

lispCons :: PF
lispCons (LispList items@(x:xs))
  | length items == 2 = LispPair (head items) (last items)
  | otherwise = foldr LispPair LispUnit items
lispCons _ = LispError "Error: cons must be applied to a list"

lispCar :: PF
lispCar (LispPair a b) = a
lispCar _ = LispError "Error: car must be applied to a pair"

lispCdr :: PF
lispCdr (LispPair a b) = b
lispCdr _ = LispError "Error: cdr must be applied to a pair"

primPlus :: PF
primPlus (LispList items) = foldr lispAdd (LispInt 0) items
primPlus _ = LispError "Error: + must be applied to a list"

primNeg :: PF
primNeg = lispNegate

primMult :: PF
primMult (LispList items) = foldr lispMult (LispInt 1) items
primMult _ = LispError "Error: * must be applied to a list"

primInvert :: PF
primInvert = lispInvert

primitives :: [Var]
primitives = [
  Var "+" $ LispPrimitive $ LP primPlus,
  Var "*" $ LispPrimitive $ LP primMult,
  Var "-" $ LispPrimitive $ LP primNeg,
  Var "/" $ LispPrimitive $ LP primInvert,
  Var "cons" $ LispPrimitive $ LP lispCons,
  Var "car" $ LispPrimitive $ LP lispCar,
  Var "cdr" $ LispPrimitive $ LP lispCdr
             ]

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
apply (LispPrimitive (LP fn)) args env =
  let args' = map (\a -> envExpr $ return env >>= interpret a) args
  in Environment (case fn of
                    primInvert -> if length args' == 1 then fn $ head args' else fn LispUnit
                    primMult -> fn (LispList args')
                    primPlus -> fn $ LispList args'
                    primNeg -> if length args' == 1 then fn $ head args' else fn LispUnit
                    lispCons -> fn $ LispList args'
                    lispCar -> if length args' == 1 then fn (head args') else fn LispUnit
                    lispCdr -> if length args' == 1 then fn (head args') else fn LispUnit) env
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

testFn'' :: Environment Env
testFn'' = return primitives
  >>= run "(- 1)"
