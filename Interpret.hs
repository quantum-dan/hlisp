module Interpret
  (
    Environment (..),
    Env (..),
    run,
    interpret
  ) where

import Parse
import Primitives

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
  (Environment le@(LispError _) env) >>= f = (f env) {envExpr = le}
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
  Left err -> Environment (LispError err) env
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
testFn = return primitives
  >>= run "(def var1 5)"
  >>= run "(def var2 6)"
  >>= run "(def var3 5)"
  >>= run "(cons (= var2 var1) (= var3 var1) (if (= var2 var1) #t #f))"

testFn' :: Environment Env
testFn' = return primitives
  >>= run "(def var 'hello)"
  >>= run "(cons 1 var \"fish\")"
  >>= run "(* 1.0 2 3 (negate 4) (invert 3.0))"
  >>= run "(def fn cdr)"
  >>= run "(def val (cons 1 2))"
  >>= run "(fn val)"
