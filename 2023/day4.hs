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

solvep2 :: [String] -> [Int]
solvep2 s = take (cardCount * 2) . foldl update (replicate cardCount 1 ++ repeat 0) $ zip [1 ..] matchesPerCard
  where
    update :: [Int] -> (Int, Int) -> [Int]
    update acc (idx, curr) = zipWith (+) acc (replicate idx 0 ++ replicate curr 1 ++ repeat 0)

    matchesPerCard = map (winningCards . formatInput) s
    cardCount = length matchesPerCard

day4p2 :: IO [Int]
day4p2 = do
  vals <- lines <$> readFile "./ts.txt" :: IO [String]
  return $ solvep2 vals

-- ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53","Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19","Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1","Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83","Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36","Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]

-- 4 -> 1
-- 2 -> 2
-- 2 -> 4
-- 1 -> 8
-- 0 -> 14
