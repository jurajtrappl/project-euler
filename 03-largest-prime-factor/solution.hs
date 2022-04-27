value :: Integer 
value = 600851475143

sieve :: Integer -> [Integer]
sieve n = [x | x <- [2..n], and [x `mod` y /= 0 | y <- [2..floor(sqrt(fromIntegral x))]]]

main :: IO ()
main = do
    print $ filter (\ n -> value `mod` n == 0) $ sieve value