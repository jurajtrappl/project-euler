import Data.Char (digitToInt)

main :: IO ()
main = do
    print $ sum $ map digitToInt $ show $ product [1..100]