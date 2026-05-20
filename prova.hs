{- Faça, em Haskell, uma solução que receba um Int a e uma [Int] b. Como saída,
teremos um Bool que informa se a ∈ b.
f_in::Int->[Int]->Bool -}

fin :: Int -> [Int] -> Bool

fin x (a:b)
    |x == a = True
    |x /= a = fin x b

{-(20 pt) Considerando f_in, faça, em Haskell, uma solução que receba um Int x e uma [[Int]] l.
Como saída, teremos uma [(Bool, [Int])] s que informa, para cada sublista li de l, se o elemento
x pertence ou não à li
.
f2::Int->[[Int]]->[(Bool,[Int])]-}


    
