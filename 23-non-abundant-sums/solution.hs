value :: Int
value = 28123

divisors :: Integral a => a -> [a]
divisors n = [x | x <- [1..n - 1], n `mod` x == 0]

isAbundant :: Integral a => a -> Bool
isAbundant n = sum (tail $ divisors n) > n

sumOfAbundants :: [Int]
sumOfAbundants = [a + b | (a, b) <- [(x, y) | x <- abundants, y <- abundants], a <= b]
    where abundants = filter isAbundant [1..value]

main :: IO ()
main = do
    let abundant = filter isAbundant [1..value]
    print $ sum $ filter (`notElem` sumOfAbundants) [1..value]
