import Data.Text.Internal.Read (digitToInt)

splitToChunks :: [a] -> [[a]]
splitToChunks value@(x:xs) = if length value > 13
                                then take 13 value : splitToChunks xs
                                else [value]

main :: IO ()
main = do
    unprocInput <- readFile "input.in"
    let input = concat $ lines unprocInput
    print $ maximum $ map (product . map digitToInt) $ splitToChunks input