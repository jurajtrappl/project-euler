import qualified Data.Set as S

main :: IO ()
main = do
    print $ length $ S.fromList [a ^ b | a <- [2..100], b <- [2..100]]