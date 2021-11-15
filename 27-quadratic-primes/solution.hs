import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Data.Function (on)

coefficients :: [(Int, Int)]
coefficients = [(a, b) | a <- [-999..999], b <- [-1000..1000]]

computeNumber :: (Int, Int) -> Int -> Int
computeNumber (a, b) n = n ^ 2 + a * n + b

primes :: [Int]
primes = [x | x <- [2..100000], and [x `mod` y /= 0 | y <- [2..floor(sqrt(fromIntegral x))]]]

main :: IO ()
main = do
    print $ maximumBy (compare `on` length . snd)
            $ map (\ coef -> (coef, takeWhile (`elem` primes) $ map (computeNumber coef) [0..1000])) coefficients
