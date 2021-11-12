triangular :: (Num a, Enum a) => a -> a
triangular n = sum [1..n]

factors :: Integral a => a -> [a]
factors n = [ x | x <- [1..floor(sqrt(fromIntegral n))], n `mod` x == 0]

main :: IO ()
main = do
    print $ head $ filter (\ t -> length (factors t) >= 250) $ map triangular [1..1000000]