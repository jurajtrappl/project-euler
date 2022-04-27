main :: IO ()
main = do
    print $ head $ [a * b * c |
                        c <- [1..],
                        b <- [1..c - 1],
                        a <- [1..b - 1],
                        a + b + c == 1000,
                        a ^ 2 + b ^ 2 == c ^ 2]