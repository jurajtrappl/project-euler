isEvenlyDivisibleByNums :: Integral a => a -> Bool
isEvenlyDivisibleByNums n = all (\ x -> n `mod` x == 0) [2..20]

main :: IO ()
main = do
    print $ take 1 $ filter isEvenlyDivisibleByNums [100000000..]