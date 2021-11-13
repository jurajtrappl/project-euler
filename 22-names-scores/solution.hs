import Data.List.Split ( splitOn )
import Data.List ( elemIndex, sort )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )

alphabetValues :: M.Map Char Int
alphabetValues = M.fromList $ zip ['A'..'Z'] [1..26]

wordCharValue :: String -> Int
wordCharValue = sum . map (alphabetValues M.!)

wordIndex :: String -> [String] -> Int
wordIndex w words = fromMaybe 0 (elemIndex w words) + 1

main :: IO ()
main = do
    input <- readFile "input.in"
    let names = splitOn "," $ filter (/= '"') input
    let sorted = sort names
    print $ sum $ map (\ w -> wordCharValue w * wordIndex w sorted) sorted
