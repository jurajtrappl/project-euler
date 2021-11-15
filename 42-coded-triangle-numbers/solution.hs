import Data.List.Split ( splitOn )
import qualified Data.Map as M

nthTriangle :: Int -> Int
nthTriangle n = div (n * (n + 1)) 2

alphabetValues :: M.Map Char Int
alphabetValues = M.fromList $ zip ['A'..'Z'] [1..26]

wordCharValue :: String -> Int
wordCharValue = sum . map (alphabetValues M.!)

main :: IO ()
main = do
    input <- readFile "input.in"
    let w = splitOn "," $ filter (/= '"') input
    let firstHundredTriangles = map nthTriangle [1..100]
    print $ length $ filter (`elem` firstHundredTriangles) $ map wordCharValue w