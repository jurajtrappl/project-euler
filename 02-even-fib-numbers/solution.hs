fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

upperBound :: Integer
upperBound = 4000000

main :: IO ()
main = do
    print $ sum $ filter even $ takeWhile (<upperBound) fib