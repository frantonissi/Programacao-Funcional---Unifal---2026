howManyEqual :: Int -> Int -> Int -> Int
howManyEqual a b c
    |(a == b) && (b /= c) = 2
    |(a /= b) && (a /= c) && (b /= c) = 0
    |(a /= b) && (b == c) = 2
    |otherwise = 3