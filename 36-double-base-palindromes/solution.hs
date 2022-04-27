toBinary :: Int -> [Int]
toBinary n = inBinary
    where
        inBinary = toBinaryHelper n
        toBinaryHelper n 
            | n /= 0 = mod n 2 : toBinaryHelper (div n 2)
            | otherwise = []

isPalindromeBaseTen :: Int -> Bool
isPalindromeBaseTen n = reverse (show n) == show n

isPalindromeBinary :: [Int] -> Bool
isPalindromeBinary n = n == reverse n

main :: IO ()
main = do
    print $ sum $ filter (\ n -> isPalindromeBaseTen n && isPalindromeBinary (toBinary n)) [1..999999]