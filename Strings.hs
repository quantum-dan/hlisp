module Strings
where

groupBy :: ([a] -> Maybe ([a], [a])) -> [a] -> Maybe [[a]]
groupBy groups items = groupBy items groups []
  where
    groupBy [] _ accum = Just accum
    groupBy items groups accum = do
      (group, rest) <- groups items
      groupBy rest groups (accum ++ [group])

split :: Eq a => a -> [a] -> [[a]]
split compare vals = split' compare vals [] [] where
  split' _ [] item accum = accum ++ [item]
  split' compare (x:xs) item accum
    | x == compare = split' compare xs [] (accum ++ [item])
    | otherwise = split' compare xs (item ++ [x]) accum

inBetween :: (Eq a) => a -> a -> [a] -> Maybe [a]
inBetween s e xs = fmap fst $ inBetweenRem s e xs

inBetweenRem :: (Eq a) => a -> a -> [a] -> Maybe ([a], [a])
inBetweenRem start end vals = (after start vals) >>= (\v -> inner start end v 1 [])
  where
    inner :: (Eq a) => a -> a -> [a] -> Int -> [a] -> Maybe ([a], [a])
    inner _ _ xs 0 accum = Just (accum, xs)
    inner _ _ [] _ _ = Nothing
    inner start end (x:xs) count accum
      | x == start = inner start end xs (count + 1) (accum ++ [x])
      | x == end && count == 1 = Just (accum, xs)
      | x == end = inner start end xs (count - 1) (accum ++ [x])
      | otherwise = inner start end xs count (accum ++ [x])

inBetween' :: (Eq a) => a -> [a] -> Maybe [a]
inBetween' compare xs = (after compare xs) >>= (Strings.until compare)

until :: (Eq a) => a -> [a] -> Maybe [a]
until compare vals = until' compare vals []
  where
    until' :: (Eq a) => a -> [a] -> [a] -> Maybe [a]
    until' _ [] _ = Nothing
    until' compare (x:xs) accum
      | compare == x = Just accum
      | otherwise = until' compare xs (accum ++ [x])

after :: (Eq a) => a -> [a] -> Maybe [a]
after _ [] = Nothing
after compare (x:xs)
  | compare == x = Just xs
  | otherwise = after compare xs

afterLast :: (Eq a) => a -> [a] -> [a]
afterLast compare vals = inner compare vals []
  where
    inner _ [] accum = accum
    inner compare (x:xs) accum
      | x == compare = inner compare xs []
      | otherwise = inner compare xs (accum ++ [x])
