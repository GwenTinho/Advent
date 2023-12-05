import Data.Char (isDigit, isSpace)
import Data.List (intersect)
import Data.Text (pack, splitOn, unpack)

splitOnS :: String -> String -> [String]
splitOnS delimiter s = map unpack $ splitOn (pack delimiter) (pack s)

toPair :: [a] -> (a, a)
toPair l = (head l, head (tail l))

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

formatInput :: String -> ([Int], [Int])
formatInput = toPair . map ((map read . filter (/= "") . map trim . splitOnS " ") . trim) . splitOnS "|" . drop 2 . dropWhile (/= ':')

winningCards :: ([Int], [Int]) -> Int
winningCards = length . uncurry intersect

solvep1 :: [String] -> Int
solvep1 = sum . map ((\x -> if x == 0 then 0 else 2 ^ (x - 1)) . winningCards . formatInput)

day4p1 :: IO Int
day4p1 = do
  vals <- lines <$> readFile "./input4.txt" :: IO [String]
  return $ solvep1 vals

solvep2 :: [String] -> Int
solvep2 s = sum $ aux (replicate cardCount 1) 0 matchesPerCard
  where
    aux :: [Int] -> Int -> [Int] -> [Int]
    aux acc idx [] = acc
    aux acc idx (matchCount : t) =
      let newacc = zipWith (+) acc (replicate (idx + 1) 0 ++ replicate matchCount (acc !! idx) ++ repeat 0)
       in aux newacc (idx + 1) t

    matchesPerCard = map (winningCards . formatInput) s
    cardCount = length matchesPerCard

day4p2 :: IO Int
day4p2 = do
  vals <- lines <$> readFile "./input4.txt" :: IO [String]
  return $ solvep2 vals
