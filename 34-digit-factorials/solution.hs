import qualified Data.Map as M

digitFactorials :: M.Map Int Int
digitFactorials = M.fromList [(0, 1), (1, 1), (2, 2), (3, 6), (4, 24), (5, 120), (6, 720), (7, 5040), (8, 40320), (9, 362880)]

digitFactorialSum :: Int -> Int
digitFactorialSum n
    | n /= 0 = digitFactorials M.! mod n 10 + digitFactorialSum (div n 10)
    | otherwise = 0

main :: IO ()
main = do
    print $ sum $ filter (\ n -> digitFactorialSum n == n) [3..1000000]