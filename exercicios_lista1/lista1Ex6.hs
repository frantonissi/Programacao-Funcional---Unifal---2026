raiz :: Float -> Float
raiz x = sqrt 6

soma :: Int -> Float
soma 0 = sqrt 6
soma m = raiz (fromIntegral m) + soma(m - 1)

main :: IO()

main = do
    print(soma 4)