import qualified Data.Map as M

properDivisors :: Integral a => a -> [a]
properDivisors n = [x | x <- [1..n-1], n `mod` x == 0]

areAmicable :: Integral a => M.Map a a -> (a, a) -> Bool
areAmicable precomputed (x, y) = precomputed M.! x == y && precomputed M.! y == x

main :: IO ()
main = do
    let precomputedSums = M.fromList [(x, sum (properDivisors x)) | x <- [1..10000]]
    let nums = [(a,b) | (a,b) <- [(x, y) | x <- [1..10000], y <- [1..10000]], a < b]
    print $ sum $ map (uncurry (+)) $ filter (areAmicable precomputedSums) nums