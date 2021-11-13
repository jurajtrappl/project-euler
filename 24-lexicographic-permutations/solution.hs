perm :: [Int]
perm = [0..9]

takeIncreasing :: [Int] -> [Int]
takeIncreasing (x:y:xs)
    | x <= y = x : takeIncreasing (y:xs)
    | otherwise = [x]
takeIncreasing xs = xs

splitByList :: [Int] -> [Int] -> [Int]
splitByList src@(x:xs) (y:ys)
    | x == y = splitByList xs ys
    | otherwise = src
splitByList xs [] = xs

rightMostSucc :: [Int] -> Int -> Int
rightMostSucc (x:xs) p
    | x < p = rightMostSucc xs p
    | otherwise = x

swapRightMostSucc :: [Int] -> Int -> [Int]
swapRightMostSucc (x:xs) p
    | x < p = x : swapRightMostSucc xs p
    | otherwise = p:xs

next :: [Int] -> [Int]
next p = init fstPart ++ [succ] ++ swapped
    where longestNonIncreasingSuffix = reverse $ takeIncreasing (reverse p)
          fstPart = reverse $ splitByList (reverse p) (reverse longestNonIncreasingSuffix)
          pivot = last fstPart
          succ = rightMostSucc (reverse longestNonIncreasingSuffix) pivot
          swapped = swapRightMostSucc (reverse longestNonIncreasingSuffix) pivot

repeatN :: (a -> a) -> a -> Int -> a
repeatN f p 0 = p
repeatN f p n = repeatN f (f p) (n-1)

main :: IO [Int]
main = do
    return $ repeatN next perm 999999