module Parse where

inBetween :: (Eq a) => a -> a -> [a] -> Maybe [a]
inBetween start end vals = (after start vals) >>= (\v -> inner start end v 1 [])
  where
    inner :: (Eq a) => a -> a -> [a] -> Int -> [a] -> Maybe [a]
    inner _ _ _ 0 accum = Just accum
    inner _ _ [] _ _ = Nothing
    inner start end (x:xs) count accum
      | x == start = inner start end xs (count + 1) (accum ++ [x])
      | x == end && count == 1 = Just accum
      | x == end = inner start end xs (count - 1) (accum ++ [x])
      | otherwise = inner start end xs count (accum ++ [x])

inBetween' :: (Eq a) => a -> [a] -> Maybe [a]
inBetween' compare xs = (after compare xs) >>= (Parse.until compare)

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
