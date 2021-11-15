import Data.List ( intersect )

nthTriangular :: Int -> Int
nthTriangular n = div (n * (n + 1)) 2

nthPentagonal :: Int -> Int
nthPentagonal n = div (n * (3 * n - 1)) 2

nthHexagonal :: Int -> Int
nthHexagonal n = n * (2 * n - 1)

main :: IO ()
main = do
    let triangular = map nthTriangular [1..100000]
    let pentagonal = map nthPentagonal [1..100000]
    let hexagonal = map nthHexagonal [1..100000]
    print $ triangular `intersect` pentagonal `intersect` hexagonal