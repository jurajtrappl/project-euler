readInteger :: String -> Integer
readInteger s = read s :: Integer

main :: IO ()
main = do
    unprocInput <- readFile "input.in"
    let input = lines unprocInput
    print $ take 10 $ show $ sum $ map readInteger input