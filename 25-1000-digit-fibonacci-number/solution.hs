fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = do
    print $ length $ takeWhile (\ x -> length (show x) < 1000) fibs