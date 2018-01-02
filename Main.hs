module Main where

import Parse
import Interpret
import Primitives (primitives)
import Data.List (isPrefixOf)
import System.IO (readFile)
import System.Environment (getArgs)

main :: IO ()
main = runFromArgs

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

runFromArgs :: IO ()
runFromArgs = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Lisp REPL - enter ,q to exit"
      runStdIn primitives
    ["-h"] -> putStrLn infoStr
    [fn] -> runFile fn >>= putStrLn
    "-e":[code] -> putStrLn $ runString code
    "-l":[fn] -> loadFile fn

loadFile :: String -> IO ()
loadFile fn = do
  result <- execFile fn primitives
  case result of
    Just (Environment ld env) -> runStdIn env
    Nothing -> putStrLn "Failed to parse code"

runStdIn :: Env -> IO ()
runStdIn env = do
  str <- getLine
  if str `elem` [",q", ",quit", ",exit"]
    then return ()
    else do
      let (Environment ld env') = run str env
      putStrLn $ show ld ++ "\n"
      runStdIn env'

infoStr :: String
infoStr =
  "lisp -h|<filename>|-e [code]\n" ++
  "HLisp: Haskell Lisp interpreter\n" ++
  "Options:\n" ++
  "\t-h\t\tDisplay this help menu\n" ++
  "\t<filename>\tExecute the given file\n" ++
  "\t-e <code>\tExecute the given code" ++
  "\t Default: launches REPL"
