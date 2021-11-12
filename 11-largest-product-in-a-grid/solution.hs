import qualified Data.Matrix as M

traverseLeft :: M.Matrix Int -> (Int, Int) -> Int
traverseLeft m (r, c) = if c - 3 > 0
                                then product [m M.! (r, c), m M.! (r, c - 1), m M.! (r, c - 2), m M.! (r, c - 3)]
                                else 0

traverseRight :: M.Matrix Int -> (Int, Int) -> Int
traverseRight m (r, c) = if c + 3 <= M.ncols m
                                 then product [m M.! (r, c), m M.! (r, c + 1), m M.! (r, c + 2), m M.! (r, c + 3)]
                                 else 0

traverseDown :: M.Matrix Int -> (Int, Int) -> Int
traverseDown m (r, c) = if r + 3 <= M.nrows m
                                then product [m M.! (r, c), m M.! (r + 1, c), m M.! (r + 2, c), m M.! (r + 3, c)]
                                else 0

traverseUp :: M.Matrix Int -> (Int, Int) -> Int
traverseUp m (r, c) = if r - 3 > 0
                                then product [m M.! (r, c), m M.! (r - 1, c), m M.! (r - 2, c), m M.! (r - 3, c)]
                                else 0

traverseDiagonalDownLeft :: M.Matrix Int -> (Int, Int) -> Int
traverseDiagonalDownLeft m pos@(r, c) = if r + 3 <= M.nrows m && c - 3 > 0
                                                  then product [m M.! (r, c), m M.! (r + 1, c - 1), m M.! (r + 2, c - 2), m M.! (r + 3, c - 3)]
                                                  else 0

traverseDiagonalDownRight :: M.Matrix Int -> (Int, Int) -> Int
traverseDiagonalDownRight m pos@(r, c) = if r + 3 <= M.nrows m && c + 3 <= M.ncols m
                                                    then product [m M.! (r, c), m M.! (r + 1, c + 1), m M.! (r + 2, c + 2), m M.! (r + 3, c + 3)]
                                                    else 0

traverseMatrix :: M.Matrix Int -> (Int, Int) -> [Int]
traverseMatrix matrix pos@(r, c)
    | M.nrows matrix == r && M.ncols matrix == c = []
    | M.ncols matrix == c = traverseMatrix matrix (r + 1, 1)
    | otherwise = filter (/= 0) [traverseLeft matrix pos,
                   traverseRight matrix pos,
                   traverseDown matrix pos,
                   traverseUp matrix pos,
                   traverseDiagonalDownLeft matrix pos,
                   traverseDiagonalDownRight matrix pos] ++ traverseMatrix matrix (r, c + 1)

readInt :: String -> Int
readInt value = read value :: Int

main :: IO ()
main = do
    input <- readFile "input.in"
    let matrix = M.fromLists $ map (map readInt . words) $ lines input
    print $ maximum $ traverseMatrix matrix (1, 1)