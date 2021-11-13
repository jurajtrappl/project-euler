import qualified Data.Matrix as M

readInt :: String -> Int
readInt value = read value :: Int

extendWithZero :: Int -> [Int] -> [Int]
extendWithZero count list = list ++ replicate (count - length list) 0

extendByRowAndCol :: Int -> [[Int]] -> [[Int]]
extendByRowAndCol dim toExtend = replicate (dim + 1) 0 : map (0 :) toExtend

pickGreaterParent :: M.Matrix Int -> (Int, Int) -> Int
pickGreaterParent m (r, c) = if leftDiagonal > up then leftDiagonal else up
    where leftDiagonal = m M.! (r - 1, c - 1)
          up = m M.! (r - 1, c)

sumUp :: M.Matrix Int -> (Int, Int) -> M.Matrix Int
sumUp m pos@(r, c)
    | M.nrows m == r && M.ncols m == c = newM
    | M.ncols m == c = sumUp newM (r + 1, 2)
    | otherwise = sumUp newM (r, c + 1)
    where newM = M.setElem (m M.! pos + pickGreaterParent m pos) pos m

main :: IO ()
main = do
    input <- readFile "input.in"
    let nums = map (map readInt . words) $ lines input
    let maxRowLength = maximum $ map length nums
    let extendedZeros = map (extendWithZero maxRowLength) nums
    let grid = extendByRowAndCol maxRowLength extendedZeros
    let m = M.fromList (maxRowLength + 1) (maxRowLength + 1) (concat grid)
    let summed = sumUp m (2, 2)
    print $ maximum $ M.getRow (M.nrows summed) summed
    