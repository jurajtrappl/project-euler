nthPentagonal :: Int -> Int
nthPentagonal n = div (n * (3 * n - 1)) 2

pentagonal :: [Int]
pentagonal = map nthPentagonal [1..20000]

pentagonalPairs :: [(Int, Int)]
pentagonalPairs = [(x,y) | (x,y) <- [(a, b) | a <- pentagonal, b <- pentagonal], x <= y]

isPentagonal :: Int -> Bool
isPentagonal n = n `elem` pentagonal

main :: IO ()
main = do
    print $ filter (\ (a, b) -> isPentagonal (a - b) && isPentagonal (a + b)) pentagonalPairs