import Data.List (group)
import Control.Arrow ((&&&))

value :: Int
value = 28123

-- behzad.nouri stack overflow
divisors :: Integral a => a -> [a]
divisors n = foldr (go . (head &&& length)) [1] . group $ fac n 2
    where
    go (_, 0) xs = xs
    go (p, k) xs = let ys = map (* p) xs in go (p, pred k) ys ++ xs
    fac n i
        | n < i * i      = [n | n /= 1]
        | n `mod` i == 0 = i: fac (n `div` i) i
        | otherwise      = fac n $ succ i

isAbundant :: Integral a => a -> Bool
isAbundant n = sum (tail $ divisors n) > n

sumOfAbundants :: [Int]
sumOfAbundants = [a + b | (a, b) <- [(x, y) | x <- abundants, y <- abundants], a <= b]
    where abundants = filter isAbundant [1..value]

main :: IO ()
main = do
    let abundant = filter isAbundant [1..value]
    print $ sum $ filter (`notElem` sumOfAbundants) [1..value]