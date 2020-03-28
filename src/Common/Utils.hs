module Common.Utils
  ( matchAndStrip,
    startsWith,
  )
where

startsWith :: Eq a => [a] -> [a] -> Maybe [a]
startsWith xs [] = Just xs
startsWith [] _ = Nothing
startsWith (x : xs) (y : ys)
  | x == y = startsWith xs ys
  | otherwise = Nothing

matchAndStrip :: Eq a => [a] -> [a] -> Maybe [a]
matchAndStrip = flip startsWith
