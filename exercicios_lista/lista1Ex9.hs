

howManyMultiples :: Int -> Int -> Int -> Int -> Int
howManyMultiples a b c contador
    | b > c = contador
    | b `mod` a == 0 = howManyMultiples a (b + 1) c (contador + 1)
    | otherwise = howManyMultiples a (b + 1) c contador