sumOfSquares :: (Num a, Enum a) => a -> a -> a
sumOfSquares a b = sum [ x ^ 2 | x <- [a..b]]

squareOfSum :: (Num a, Enum a) => a -> a -> a
squareOfSum a b = sum [a..b] ^ 2

main :: IO ()
main = do
    print $ squareOfSum 1 100 - sumOfSquares 1 100