primes :: [Integer]
primes = [x | x <- [2..], and [x `mod` y /= 0 | y <- [2..floor(sqrt(fromIntegral x))]]]

main :: IO ()
main = do
    print $ last $ take 10001 primes