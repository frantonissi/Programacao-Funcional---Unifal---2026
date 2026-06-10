
purifica:: [Int] -> [Int]
purifica [] = []
purifica (x:xs)
    | estaPresente x xs = purifica xs
    | otherwise = x:purifica xs


estaPresente :: Int -> [Int] -> Bool
estaPresente a [] = False
estaPresente a (x:xs)
    |a == x = True
    |otherwise = estaPresente a xs
