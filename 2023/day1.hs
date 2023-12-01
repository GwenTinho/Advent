import Data.Char (digitToInt, isDigit)
import Text.Regex

firstandlast :: [a] -> [a]
firstandlast l = [head l, last l]

replaceWordsWithNumbers :: String -> String
replaceWordsWithNumbers str = foldl replaceWordWithNumber str wordsToNumbers
  where
    wordsToNumbers =
      [ ("one", "o1ne"),
        ("two", "t2wo"),
        ("three", "th3ree"),
        ("four", "f4our"),
        ("five", "f5ive"),
        ("six", "s6ix"),
        ("seven", "sev7en"),
        ("eight", "eig8ht"),
        ("nine", "ni9ne"),
        ("zero", "ze0ro")
      ]
    replaceWordWithNumber str (word, number) = subRegex (mkRegex word) str number

replaceNumber :: [Char] -> [Char]
replaceNumber s = aux s []
  where
    aux :: [Char] -> [Char] -> [Char]
    aux [] munch = munch
    aux (h : t) munch = if munch ++ [h] == val then aux t (munch ++ [h]) else val ++ t
      where
        val = replaceWordsWithNumbers (munch ++ [h])

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
  vals <- lines <$> readFile "./input1.txt" :: IO [String]
  return $ solvep1 vals

day1p2 = do
  vals <- lines <$> readFile "./input1.txt" :: IO [String]
  return $ solvep2 vals
