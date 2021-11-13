import qualified Data.Map as M

wordDesc :: M.Map Int String
wordDesc = M.fromList [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five"), (6, "six"),
                       (7, "seven"), (8, "eight"), (9, "nine"), (10, "ten"), (11, "eleven"), (12, "twelve"),
                       (13, "thirteen"), (14, "fourteen"), (15, "fifteen"), (16, "sixteen"), (17, "seventeen"),
                       (18, "eighteen"), (19, "nineteen"), (20, "twenty"), (30, "thirty"), (40, "forty"),
                       (50, "fifty"), (60, "sixty"), (70, "seventy"), (80, "eighty"), (90, "ninety"),
                       (100, "hundred"), (1000, "thousand")]

convertToWords :: Int -> [String]
convertToWords n
    | n == 1000 = wordDesc M.! 1 : [wordDesc M.! 1000]
    | mod n 100 == 0 = wordDesc M.! div n 100 : [wordDesc M.! 100]
    | n > 100 = wordDesc M.! div n 100 : wordDesc M.! 100 : "and" : convertToWords (n - div n 100 * 100)
    | n >= 21 = wordDesc M.! (div n 10 * 10) : [wordDesc M.! mod n 10 | mod n 10 /= 0]
    | n == 0 = []
    | otherwise = [wordDesc M.! n]

main :: IO ()
main = print $ sum $ map (length . concat . convertToWords) [1..1000]