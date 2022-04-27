import qualified Data.List as L

dates :: [(Int, Int, Int)]
dates = [(y, m, d) | y <- [1900..2000], m <- [1..12], d <- [1..31]]

-- april, june, sept, november has 30 days
thirtyAdjust :: [(Int, Int, Int)]
thirtyAdjust = filter (`notElem` leftovers) dates
    where leftovers = filter (\ (_, m, d) -> (m == 4 || m == 6 || m == 9 || m == 11) && d == 31) dates

isLeapYear :: Integral a => a -> Bool
isLeapYear y = (mod y 4 == 0 && mod y 100 /= 0) || mod y 400 == 0

leapYears :: [Int]
leapYears = filter isLeapYear [1901..2000]

febAdjust :: [(Int, Int, Int)]
febAdjust = filter (`notElem` leftovers) thirtyAdjust
    where leftovers = filter (\ (y, m, d) -> (isLeapYear y && m == 2 && d > 29) || (not (isLeapYear y) && m == 2 && d > 28)) thirtyAdjust

isSunday :: Integral a => Maybe a -> Bool
isSunday (Just i) = mod i 7 == 6

main :: IO [(Int, Int, Int)]
main = do
    return $ filter (\ date@(y, m, d) -> y > 1900 && d == 1 && isSunday (L.elemIndex date febAdjust)) febAdjust