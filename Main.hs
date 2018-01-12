module Main where

import Parse
import Interpret
import Primitives (primitives)
import Data.List (isPrefixOf)
import System.IO (readFile, writeFile)
import System.Environment (getArgs)
import Compile

main :: IO ()
main = runFromArgs

compile :: String -> String -> IO (Either String String)
compile inPath outPath = do
  input <- readFile inPath
  let input' = cleanUp input
  let input'' = if "((" `isPrefixOf` input' then input' else "(" ++ input' ++ ")"
  let result = Compile.compile input''
  case result of
    Left _ -> return ()
    Right haskData -> writeFile outPath haskData
  return (result >> return ("Successfully compiled " ++ inPath ++ " to " ++ outPath))

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
    "-c":[fn] -> do
      result <- Main.compile fn $ fn ++ ".hs"
      case result of
        Left err -> putStrLn err
        Right s -> putStrLn s
    "-o":out:"-c":[fn] -> do
      result <- Main.compile fn out
      case result of
        Left err -> putStrLn err
        Right s -> putStrLn s
    _ -> putStrLn infoStr

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
  "\t-h\t\t\t\tDisplay this help menu\n" ++
  "\t<filename>\t\t\tExecute the given file\n" ++
  "\t-e <code>\t\t\tExecute the given code\n" ++
  "\t-o <outfile> -c <infile>\tCompile infile to outfile\n" ++
  "\t-c <file>\t\t\tCompile infile to infile.hs\n" ++
  "\t Default: launches REPL"
