primes :: [Int]
primes = [x | x <- [2..1000000], and [x `mod` y /= 0 | y <- [2..floor(sqrt(fromIntegral x))]]]

-- http://www.thetopsites.net/article/58124431.shtml
circle :: String -> [String]
circle xs = let trim ys = zipWith const ys xs
            in trim . map trim . iterate tail . cycle $ xs

readInt :: String -> Int
readInt value = read value :: Int

makePerms :: Int -> [Int]
makePerms n = map readInt $ circle (show n)

main :: IO ()
main = do
    print $ length $ filter (all (`elem` primes) . makePerms) primes