import Data.Bifunctor (first, second)
import Data.Char (isDigit)
import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set

data Alph = Dot | Symbol | Digit Int deriving (Show, Eq, Ord)

valAt :: Int -> Int -> [((Int, Int), a)] -> Maybe a
valAt i j = fmap snd . find (\((x, y), _) -> x == i && y == j)

isValid :: Alph -> Bool
isValid (Digit _) = True
isValid Symbol = True
isValid Dot = False

isDigitAlph :: Alph -> Bool
isDigitAlph (Digit _) = True
isDigitAlph _ = False

formatInput :: Int -> String -> [((Int, Int), Alph)]
formatInput l = filter (isValid . snd) . map (second charToAlph) . zipWith (curry (first (toCoord l))) [0 ..]
  where
    toCoord :: Int -> Int -> (Int, Int)
    toCoord l i = (i `mod` l, i `div` l)

    charToAlph :: Char -> Alph
    charToAlph c
      | isDigit c = Digit (read [c])
      | c == '.' = Dot
      | otherwise = Symbol

formatInputP2 :: Int -> String -> [((Int, Int), Alph)]
formatInputP2 l = filter (isValid . snd) . map (second charToAlph) . zipWith (curry (first (toCoord l))) [0 ..]
  where
    toCoord :: Int -> Int -> (Int, Int)
    toCoord l i = (i `mod` l, i `div` l)

    charToAlph :: Char -> Alph
    charToAlph c
      | isDigit c = Digit (read [c])
      | c == '*' = Symbol
      | otherwise = Dot

isSymbol :: Alph -> Bool
isSymbol = (== Symbol)

dist :: Int -> Int -> Int
dist a b = abs (a - b)

nextTo :: ((Int, Int), Alph) -> ((Int, Int), Alph) -> Bool
nextTo (_, _) (_, Dot) = False
nextTo (_, Dot) (_, _) = False
nextTo a@((i1, j1), v1) b@((i2, j2), v2) = dist i1 i2 <= 1 && dist j1 j2 <= 1 && isValid v1 && isValid v2

-- idea use internal graph representation, that is a function that says whether two elements are neighbours
-- run dfs until there are no more connected components
-- if a component contains no symbols it wont be part of it
-- if it does convert the digits into a whole number according to their position and sum them up

nubOrd :: (Ord a) => [a] -> [a]
nubOrd xs = go Set.empty xs
  where
    go s (x : xs)
      | x `Set.member` s = go s xs
      | otherwise = x : go (Set.insert x s) xs
    go _ _ = []

dfs :: (Eq a) => (a -> a -> Bool) -> [a] -> a -> [a]
dfs adjacent verticies = aux []
  where
    aux visited node =
      if node `notElem` visited
        then
          let s = filter (adjacent node) verticies
           in foldl aux (node : visited) s
        else visited

validComponents :: [((Int, Int), Alph)] -> [[((Int, Int), Alph)]]
validComponents l = nubOrd $ map (dfs nextTo l) symbols
  where
    symbols = filter (isSymbol . snd) l

gearNeighbours :: [((Int, Int), Alph)] -> [[((Int, Int), Alph)]]
gearNeighbours l = map reach symbols
  where
    symbols = filter (isSymbol . snd) l

    adj :: ((Int, Int), Alph) -> ((Int, Int), Alph) -> ((Int, Int), Alph) -> Bool
    adj v x y
      | x == v && y == v = True
      | x == v && isDigitAlph (snd y) = nextTo v y
      | y == v && isDigitAlph (snd x) = nextTo v x
      | isDigitAlph (snd y) && isDigitAlph (snd x) = nextTo y x
      | otherwise = False

    reach :: ((Int, Int), Alph) -> [((Int, Int), Alph)]
    reach v = dfs (adj v) l v

combineconseq :: Int -> Int -> [((Int, Int), Alph)] -> [Int]
combineconseq columns rows cells =
  map (read . reverse) $
    concatNeighbours columns rows 0 0 "" [] numericCells
  where
    numericCells = mapMaybe maybeDigit cells

    maybeDigit :: ((Int, Int), Alph) -> Maybe ((Int, Int), String)
    maybeDigit ((i, j), Digit v) = Just ((i, j), show v)
    maybeDigit _ = Nothing

    concatNeighbours :: Int -> Int -> Int -> Int -> String -> [String] -> [((Int, Int), String)] -> [String]
    concatNeighbours columns rows x y current acc ls
      | y == rows && x == columns = acc
      | x == columns = concatNeighbours columns rows 0 (y + 1) current acc ls
      | otherwise =
          case find ((== (x, y)) . fst) ls of
            Nothing -> if current == "" then concatNeighbours columns rows (x + 1) y [] acc ls else concatNeighbours columns rows (x + 1) y [] (current : acc) ls
            Just (_, s) -> concatNeighbours columns rows (x + 1) y (s ++ current) acc ls

-- solvep1 :: [[String]] -> [Int]
solvep1 :: [String] -> Int
solvep1 s = sum . map (sum . combineconseq columns rows) $ validComponents $ formatInput columns input
  where
    input = concat s
    rows = length s
    columns = length $ head s

-- solvep2 :: [String] -> Int
solvep2 s = sum . map product . filter ((2 ==) . length) $ map (combineconseq columns rows) $ gearNeighbours $ formatInputP2 columns input
  where
    input = concat s
    rows = length s
    columns = length $ head s

day3p1 :: IO Int
day3p1 = do
  vals <- lines <$> readFile "./input3.txt" :: IO [String]
  return $ solvep1 vals

day3p2 :: IO Int
day3p2 = do
  vals <- lines <$> readFile "./input3.txt" :: IO [String]
  return $ solvep2 vals
