module Main where

import Parse
import Interpret
import Primitives (primitives)
import Data.List (isPrefixOf)

main :: IO ()
main = putStrLn "Hello, world!"

exec :: String -> Env -> Maybe (Environment Env)
exec string env = do
  let string' = if "((" `isPrefixOf` string then string else '(':string ++ ")"
  strings <- mkList string'
  return $ foldl (\a b -> a >>= run b) (return env) strings

runString :: String -> String
runString str = case exec str primitives of
  Just (Environment ld env) -> show ld
  Nothing -> "Failed to parse code"
