import Data.Char (isDigit, isSpace)

data Category = Soil | Fertilizer | Water | Seed | Light | Temperature | Humidity | Location deriving (Eq, Show, Ord)

data Range = Range Category Int Int Int deriving (Eq, Show, Ord)

data PreMapping = PreMapping Range Range deriving (Eq, Show, Ord)

newtype Mapping = Mapping [(Int, Int)] deriving (Eq, Show, Ord)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

mappify :: PreMapping -> Mapping
mappify = undefined

formatInput :: [String] -> ([Int], [PreMapping])
formatInput ss = undefined
  where
    initialSeeds = trim . dropWhile (not . isDigit) . head $ ss

-- idea for this one is to take the initial input and keep pruning the next layer with each guess
-- on failure backtrack
