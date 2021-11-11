sieve :: Integer -> [Integer]
sieve n = [x | x <- [2..n], and [x `mod` y /= 0 | y <- [2..floor(sqrt(fromIntegral x))]]]

main :: IO ()
main = do
    print $ sum $ sieve 2000000