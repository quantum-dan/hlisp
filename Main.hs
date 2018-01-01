module Main where

import Parse
import Interpret
import Primitives (primitives)
import Data.List (isPrefixOf)
import System.IO (readFile)
import System.Environment (getArgs)

main :: IO ()
main = runFromArgs >>= putStrLn

exec :: String -> Env -> Maybe (Environment Env)
exec string env = do
  let string' = cleanUp string
  let string'' = if "((" `isPrefixOf` string' then string' else '(':string' ++ ")"
  strings <- mkList string''
  return $ foldl (\a b -> a >>= run b) (return env) strings

showMaybeEnv :: Maybe (Environment Env) -> String
showMaybeEnv (Just (Environment ld env)) = show ld
showMaybeEnv Nothing = "Failed to parse code"

runString :: String -> String
runString str = (showMaybeEnv $ exec str primitives)

execFile :: String -> Env -> IO (Maybe (Environment Env))
execFile path env = fmap (\s -> exec s env) $ readFile path

runFile :: String -> IO String
runFile path = fmap showMaybeEnv $ execFile path primitives

runFromArgs :: IO String
runFromArgs = do
  args <- getArgs
  case args of
    [] -> return infoStr
    ["-h"] -> return infoStr
    [fn] -> runFile fn
    "-e":[code] -> return $ runString code

infoStr :: String
infoStr =
  "lisp -h|<filename>|-e [code]\n" ++
  "HLisp: Haskell Lisp interpreter\n" ++
  "Options:\n" ++
  "\t-h\t\tDisplay this help menu\n" ++
  "\t<filename>\tExecute the given file\n" ++
  "\t-e <code>\tExecute the given code"
