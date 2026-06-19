-- Exercicio 1 feito completamente sozinho --
aux :: Int  -> [Int]
aux x  = [(a*a)|a <- [1..x]]

a = aux 100

quadrado :: [Int] -> Int
quadrado [] = 0
quadrado (a:b) = a + quadrado b

--Ex 1 um pouquinho melhor--
aux1 :: Int  -> [Int]
aux1 x  = [(a*a)|a <- [1..x]]

soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

quadrado1 :: Int -> Int
quadrado1 x= soma(aux1 x)


--Ex 1 perfeito--
ex1 :: Int  -> Int
ex1 x  = sum [a*a|a <- [1..x]]

--Ex 2--
replicate1 :: Int -> a -> [a]
replicate1 x a= [a|b <- [1..x]]




