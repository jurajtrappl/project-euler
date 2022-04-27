import qualified Data.Graph as G
import qualified Data.Matrix as M

latticeDim :: (Int, Int)
latticeDim = (20, 20)

lattice :: [[Int]]
lattice = replicate (fst latticeDim) (replicate (snd latticeDim) 0)

extendedLattice :: [[Int]]
extendedLattice = replicate (fst latticeDim + 1) 1 : map (1 :) lattice

exLatticeMatrix :: M.Matrix Int
exLatticeMatrix = M.fromList 21 21 (concat extendedLattice)

countPaths :: M.Matrix Int -> (Int, Int) -> Int
countPaths m pos@(r, c)
    | M.nrows m == r && M.ncols m == c = newValue
    | M.ncols m == c = countPaths newM (r + 1, 2)
    | otherwise = countPaths newM (r, c + 1)
    where newValue = m M.! (r - 1, c) + m M.! (r, c - 1)
          newM = M.setElem newValue (r, c) m

main :: IO ()
main = do
    print $ countPaths exLatticeMatrix (2, 2)