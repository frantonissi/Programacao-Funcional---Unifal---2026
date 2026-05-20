{- Faça, em Haskell, uma solução que receba um Int a e uma [Int] b. Como saída,
teremos um Bool que informa se a ∈ b.
f_in::Int->[Int]->Bool -}

fin :: Int -> [Int] -> Bool
fin x [] = False
fin x (a:b)
    |x == a = True
    |x /= a = fin x b

{-(20 pt) Considerando f_in, faça, em Haskell, uma solução que receba um Int x e uma [[Int]] l.
Como saída, teremos uma [(Bool, [Int])] s que informa, para cada sublista li de l, se o elemento
x pertence ou não à li
.-}
f2::Int->[[Int]]->[(Bool,[Int])] 
f2 x [] = []
f2 x (a:c) = (fin x a, a) : f2 x c
    -- |fin x a = (True, a) : f2 x c
    -- |otherwise = (False, a) : f2 x c

-- 3. (20 pt) Considerando f2, faça, em Haskell, uma solução que receba um Int x e uma [[Int]] l.
-- Como saída, teremos uma (Int, [(Bool, [Int])]) s que informa o Int x e, para cada sublista li
-- de l, se o elemento x pertence ou não à li.

f3::Int->[[Int]]->(Int,[(Bool,[Int])])
f3 x a = (x, f2 x a)

-- 4. (20 pt) Faça, em Haskell, uma solução que receba uma (Int,[(Bool,[Int])]) e gere uma [[Int]]
-- com apenas as listas [Int] cujo Bool da dupla é False.

f4::(Int,[(Bool,[Int])])->[[Int]]
f4 (_, []) = []
f4 (x, ((a,b) : c))
    |a == False = b : f4 (x, c)
    |otherwise = f4 (x,c)

--  Faça, em Haskell, uma solução que receba duas strings S e R, cuja menor tem tamanho
-- n. Sua solução deverá retornar uma lista de Bool B de tamanho n, de modo que cada Bi será
-- a informação se Si é igual ou diferente de Ri
-- , para 0 ≤ i < n.

f5 :: String -> String -> [Bool]
f5 [] _ = []
f5 _ [] = []
f5 (s:a) (r:b)
    | s == r    = True  : f5 a b
    | otherwise = False : f5 a b

   
    
separarParesEImpares :: [Int] -> (Int, Int)
separarParesEImpares lista = separarAux lista (0, 0)

separarAux :: [Int] -> (Int, Int) -> (Int, Int)
separarAux [] (imp, par) = (imp, par)
separarAux (a:b) (imp, par)
    |a `mod` 2 == 0 = separarAux b (imp, par + 1)
    |otherwise = separarAux b (imp + 1, par)


-- Função principal: recebe a lista
minMax :: [Int] -> (Int, Int)
-- Se a lista tem pelo menos um elemento (x) e o resto (xs):
minMax (x:xs) = minMaxAux xs (x, x)

minMaxAux :: [Int] -> (Int, Int) -> (Int, Int)
minMaxAux [] (menor, maior) = (menor, maior)
minMaxAux (a:c) (menor, maior)
    |a < menor = minMaxAux c (a, maior)
    |a > maior = minMaxAux c (menor, a)
    |otherwise = minMaxAux c (menor, maior)
