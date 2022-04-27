import Data.List (group, sort)

summedPythTriplets :: [Int]
summedPythTriplets = [a + b + c | (a, b, c) <- [(x, y, z) | x <- [1..1000],
                                                            y <- [1..1000],
                                                            z <- [1..1000],
                                                            x < y && y < z],
                                                            a ^ 2 + b ^ 2 == c ^ 2]

main :: IO ()
main = do
    print $ map ( \ xs -> (head xs, length xs))
            $ group
            $ filter (<= 1000)
            $ sort summedPythTriplets