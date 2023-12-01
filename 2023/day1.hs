import Data.Char (digitToInt, isDigit)
import Text.Regex

firstandlast :: [a] -> [a]
firstandlast l = [head l, last l]

wordsToNumbers :: [Char] -> Maybe Char
wordsToNumbers s
  | s == "nine" = Just '9'
  | s == "eight" = Just '8'
  | s == "seven" = Just '7'
  | s == "six" = Just '6'
  | s == "five" = Just '5'
  | s == "four" = Just '4'
  | s == "three" = Just '3'
  | s == "two" = Just '2'
  | s == "one" = Just '1'
  | otherwise = Nothing

replaceNumber :: [Char] -> [Char]
replaceNumber s = aux s []
  where
    aux :: [Char] -> [Char] -> [Char]
    aux [] munch = munch
    aux (h : t) munch = case wordsToNumbers (munch ++ [h]) of
      Nothing -> aux t (munch ++ [h])
      Just x -> x : t

replaceNumbers :: String -> String
replaceNumbers s = aux s (length s)
  where
    aux :: [Char] -> Int -> [Char]
    aux s 0 = s
    aux s i = aux (replaceNumber s) (i - 1)

solvep1 :: [String] -> Int
solvep1 = sum . map (read . firstandlast . filter isDigit)

solvep2 :: [String] -> Int
solvep2 = sum . map (read . firstandlast . filter isDigit . replaceNumbers)

day1p1 = do
  vals <- lines <$> readFile "./2023/input1.txt" :: IO [String]
  return $ solvep1 vals

day1p2 = do
  vals <- lines <$> readFile "./2023/input1.txt" :: IO [String]
  return $ solvep2 vals
