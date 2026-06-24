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

--Ex 3--
pyths :: Int -> [(Int,Int,Int)]
pyths x = [(a,b,c)|a <- [1..x], b <- [1..x], c <- [1..x], a*a + b*b == c*c]

--Ex 4--
perfects :: Int -> [Int]
perfects x = [a|a <- [1..x],a == sum[b|b <- [1..(a-1)], mod a b == 0]]

--Ex 5--
f1 = [(x,y) | x <- [1,2,3], y <- [4,5,6]]
f2 = concat [[(x,y)| y <- [4,5,6]] | x <- [1,2,3]]
f3 = concat [[(x,y)|x <- [1,2,3]] | y <- [4,5,6]]

--Ex 6--
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
    where n = (length xs) - 1

find :: Eq a => a -> [(a, Int)] -> [Int]
find x [] = []
find x (k:ks)
    |x == fst k = [snd k]
    |otherwise = find x ks

--Ex 7--
{-
você pega o primeiro elemento de xs com o primeiro de ys → multiplica
depois o segundo com o segundo → multiplica
...
até o último
-}
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct [] [] = 0
scalarProduct [a] [b] = a*b
scalarProduct a b = sum(auxSP a b)

auxSP :: [Int] -> [Int] -> [Int]
auxSP x y = [a*b | (a,b) <- zip x y]

{-auxSP [] [] = []
auxSP [a] [b] = [a*b]
auxSP (a:as) (b:bs) = a*b : auxSP as bs-}

--Ex 8--

--Ex 9--
f9 = [odd x | x <- [1..10]] 

--Ex 10--
decInt :: [Int] -> Int
decInt [] = 0
decInt [a] = a
decInt a = foldl (\b x -> b*10 + x) 0 a 
{-O que é \b x -> b*10 + x?
Isso é uma função anônima (lambda). Lê assim:

"Dados dois valores b e x, retorna b*10 + x"


b = o acumulador (o resultado que estou carregando)
x = o elemento atual da lista-}

decInt1 :: [Int] -> Int
decInt1 [] = 0
decInt1 [a] = a
decInt1 a = foldl (\b y -> b*10+y) 0 a -- O b é o valor inicial, o y é cada valor da lista
{-\b y -> ...
 ↑   ↑
 |   |
 |   elemento da lista
 |
 acumulador-}

--Ex 11--
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)
  --o Retorno (<1024) id (*2) 1

--Ex 12--
evenCube :: Int -> [Int]
evenCube x = [a*a*a|a <- [1..x-1], even a]

--Ex 13--
insertOrd :: Int -> [Int] -> [Int]
insertOrd a x = [b|b <- x, b < a] ++ [a] ++ [b|b <- x, b >= a]

--Ex 14--

howManyMultiples :: Int -> Int -> Int -> Int
howManyMultiples a b c = length[d|d <- [b..c], mod d a == 0]

--Ex 15--
duplicate :: String -> Int -> String
duplicate _ 0 = ""
duplicate s n = foldl (\x y -> (++) x y) "" [s|a <- [1..n]] 

--Ex 16--
pushRight::String->Int->String
pushRight x y
    |length x >= y = x
    |otherwise = ['>'|z <- [1..(y - length x)]] ++ x
        
--Ex 17--
inverte :: [Int] -> [Int]
inverte x = [y|y <- x]