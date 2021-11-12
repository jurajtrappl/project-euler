import Data.Function (on)
import Data.List ( maximumBy )

evenCollatz :: Integral a => a -> a
evenCollatz n = div n 2

oddCollatz :: Num a => a -> a
oddCollatz n = 3 * n + 1

collatzSeq :: Integral a => a -> [a]
collatzSeq n
    | n /= 1 = if odd n then n : collatzSeq (oddCollatz n) else n : collatzSeq (evenCollatz n)
    | otherwise = [1]

main :: IO ()
main = do
    print $ maximumBy (compare `on` length) $ map collatzSeq [1..1000000]