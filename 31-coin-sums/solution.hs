import Data.List ( subsequences )

type Coin = Int

coins :: [Coin]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

coinsCombinations :: [[Coin]]
coinsCombinations = filter (/= []) $ subsequences coins



