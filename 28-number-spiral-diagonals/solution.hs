import Data.Matrix ( setElem, zero, Matrix, getDiag, transpose, toLists, fromLists )
import Data.List (foldl')

type Direction = Int
type Dimension = Int

squareFrame :: Dimension -> Direction -> [Int] -> [[Int]]
squareFrame 1 _ nums = [nums]
squareFrame dim 1 nums = take dim nums : squareFrame dim 2 (drop dim nums)
squareFrame dim 2 nums = take (dim - 1) nums : squareFrame dim 3 (drop (dim - 1) nums)
squareFrame dim 3 nums = take (dim - 1) nums : squareFrame dim 4 (drop (dim - 1) nums)
squareFrame dim 4 nums = take (dim - 2) nums : squareFrame (dim - 2) 1 (drop (dim - 2) nums)

createSquareFrame :: Dimension -> [[Int]]
createSquareFrame dim = squareFrame dim 1 (reverse [1..dim ^ 2])

fillMatrix :: (Int, Int) -> (Int, Int) -> [Int] -> Matrix Int -> Matrix Int
fillMatrix _ _ [] m = m
fillMatrix curr@(x1, y1) dir@(x, y) (v:vs) m = fillMatrix (x1 + x, y1 + y) dir vs (setElem v curr m)

squareMatrix :: (Int, Int) -> Direction -> [[Int]] -> Matrix Int -> Matrix Int
squareMatrix _ _ [] m = m
squareMatrix f@(x, y) 1 (n:ns) m = squareMatrix (x, y - length n + 1) 2 ns (fillMatrix f (0, -1) n m)
squareMatrix f@(x, y) 2 (n:ns) m = squareMatrix (x + length n, y) 3 ns (fillMatrix (x + 1, y) (1, 0) n m)
squareMatrix f@(x, y) 3 (n:ns) m = squareMatrix (x, y + length n) 4 ns (fillMatrix (x, y + 1) (0, 1) n m)
squareMatrix f@(x, y) 4 (n:ns) m = squareMatrix (x - length n, y - 1) 1 ns (fillMatrix (x - 1, y) (-1, 0) n m)

createSquareMatrix :: Dimension -> [[Int]] -> Matrix Int -> Matrix Int
createSquareMatrix dim = squareMatrix (1, dim) 1

reverse' :: [a] -> [a]
reverse' = foldl' (flip (:)) []

solve :: IO ()
solve = do
    let spiral = createSquareMatrix 1001 (createSquareFrame 1001) (zero 1001 1001)
    print $ sum (getDiag spiral) + sum (getDiag (fromLists(map reverse' (toLists spiral)))) - 1