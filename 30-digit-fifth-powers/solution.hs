import qualified Data.Map as M

powers :: M.Map Int Int
powers = M.fromList [(0, 0), (1, 1), (2, 32), (3, 243), (4, 1024), (5, 3125), (6, 7776), (7, 16807), (8, 32768), (9, 59049)]

digitPowerSum :: Int -> Int
digitPowerSum n
    | n /= 0 = powers M.! mod n 10 + digitPowerSum (div n 10)
    | otherwise = 0

main :: IO ()
main = do
    print $ sum $ filter (\ n -> digitPowerSum n == n) [2..1000000]