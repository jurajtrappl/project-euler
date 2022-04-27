isPalindrome :: Int -> Bool
isPalindrome n = show n == reverse (show n)

main :: IO ()
main = do
    print $ maximum $ filter isPalindrome [ x * y| x <- [100..999], y <- [100..999]]