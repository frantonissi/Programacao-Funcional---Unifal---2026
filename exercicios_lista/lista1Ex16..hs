funny :: Int -> Int -> Int -> Bool
funny x y z = x > y && x > z && x /= y
{-funny x y z
    | x > z = True
    | y >= x = False-}