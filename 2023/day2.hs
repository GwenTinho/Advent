import Data.Char (isAlpha, isDigit)
import Data.Foldable (minimumBy)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Text (pack, splitOn, unpack)

data Col = Red Int | Green Int | Blue Int deriving (Show)

data Out = Out Int Int Int deriving (Show)

newtype Id = Id Int deriving (Show)

data Game = Game Id [Out] deriving (Show)

compareOut :: Out -> Out -> Bool
compareOut (Out a b c) (Out aa bb cc) = a >= aa && b >= bb && c >= cc

validGame :: Out -> Game -> Maybe Id
validGame max (Game id outs) = if all (compareOut max) outs then Just id else Nothing

minOut :: Game -> Out
minOut (Game _ os) = Out (maximum reds) (maximum greens) (maximum blues)
  where
    (reds, greens, blues) = unzip3 $ filter (\(x, y, z) -> x + y + z /= 0) $ map (\(Out r g b) -> (r, g, b)) os

pow :: Out -> Int
pow (Out r g b) = r * g * b

splitOnS :: String -> String -> [String]
splitOnS delimiter s = map unpack $ splitOn (pack delimiter) (pack s)

unpackId :: Id -> Int
unpackId (Id i) = i

formatInput :: String -> Game
formatInput s = Game id outs
  where
    id = Id $ read $ filter isDigit $ takeWhile (/= ':') s
    rounds = splitOnS ";" (dropWhile (/= ':') s)
    pulls = map (map assignCol . splitOnS ",") rounds
    outs = map (pullToOut (Out 0 0 0)) pulls

    assignCol :: String -> Maybe Col
    assignCol s =
      let firstChar = head $ dropWhile (not . isAlpha) s
          val = read $ takeWhile isDigit $ dropWhile (not . isDigit) s :: Int
       in case firstChar of
            'g' -> Just $ Green val
            'b' -> Just $ Blue val
            'r' -> Just $ Red val
            _ -> Nothing

    pullToOut :: Out -> [Maybe Col] -> Out
    pullToOut o [] = o
    pullToOut (Out r g b) ((Just (Red v)) : t) = pullToOut (Out (r + v) g b) t
    pullToOut (Out r g b) ((Just (Green v)) : t) = pullToOut (Out r (g + v) b) t
    pullToOut (Out r g b) ((Just (Blue v)) : t) = pullToOut (Out r g (b + v)) t
    pullToOut o (Nothing : _) = o

solvep1 :: Out -> [String] -> Int
solvep1 max = sum . map unpackId . mapMaybe (validGame max . formatInput)

solvep2 :: [String] -> Int
solvep2 = sum . map (pow . minOut . formatInput)

day1p1 :: IO Int
day1p1 = do
  vals <- lines <$> readFile "./input2.txt" :: IO [String]
  return $ solvep1 (Out 12 13 14) vals

day1p2 :: IO Int
day1p2 = do
  vals <- lines <$> readFile "./input2.txt" :: IO [String]
  return $ solvep2 vals
