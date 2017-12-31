module Regex (
  Match (..),
  match,
  regex
             ) where

data Match = Start | End | SomeCharacters [Char] | MaybeCharacter [Char] | MaybeCharacters [Char] | Characters [Char] Int | MaybeNoneOf [Char] | NoneOf [Char] Int | SomeNoneOf [Char]
  deriving Read

type Regex = [Match]

contains :: (Eq a) => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) y = (y == x) || (contains xs y)

getAllIn :: (Eq a) => [a] -> [a] -> [a] -> ([a], [a])
getAllIn _ [] _ = ([], [])
getAllIn [] _ accum = (accum, [])
getAllIn total@(i:input) filters accum = if contains filters i
  then getAllIn input filters (accum ++ [i])
  else (accum, total)

until :: (Eq a) => [a] -> [a] -> ([a], [a])
until matches str = until' matches str []
  where
    until' _ [] accum = (accum, [])
    until' matches str@(x:xs) accum
      | contains matches x = (accum, str)
      | otherwise = until' matches xs (accum ++ [x])

match :: Match -> String -> Maybe (String, String)
match m [] = case m of
  Start -> Just ([], [])
  End -> Just ([], [])
  MaybeCharacter _ -> Just ([], [])
  MaybeCharacters _ -> Just ([], [])
  MaybeNoneOf _ -> Just ([], [])
  _ -> Nothing
match m string@(s:str) = case m of
  Start -> Just ([], string)
  End -> Nothing
  SomeCharacters chars -> case getAllIn string chars [] of
    ([], _) -> Nothing
    (found, rest) -> Just (found, rest)
  MaybeCharacter chars -> if contains chars s
    then Just ([s], str)
    else Just ([], string)
  MaybeCharacters chars -> Just $ getAllIn string chars []
  Characters chars len -> if length result == len then Just (result, rest) else Nothing
    where (result, rest) = getAllIn string chars []
  MaybeNoneOf chars -> Just $ Regex.until chars string
  NoneOf chars len -> if length result == len then Just (result, rest) else Nothing
    where (result, rest) = Regex.until chars string
  SomeNoneOf chars -> case Regex.until chars string of
    ([], _) -> Nothing
    (found, rest) -> Just (found, rest)

regex :: Regex -> String -> Maybe String
regex _ [] = Nothing
regex matches string = rInner False [] matches string
  where
    rInner started accum [] _ = if started then Just accum else Nothing
    rInner started accum ms@(m:matches) string =
      case match m string of
        Nothing -> if started then Nothing else
          case string of
            [] -> Nothing
            (s:str) -> rInner False accum ms str
        Just (found, rest) -> case rest of
          [] -> Just $ accum ++ found
          _ -> rInner True (accum ++ found) matches rest

main = do
  matchString <- getLine
  testString <- getLine
  let matches = read matchString :: Regex
  print $ regex matches testString
