{- Faça, em Haskell, uma solução que receba um Int a e uma [Int] b. Como saída,
teremos um Bool que informa se a ∈ b.
f_in::Int->[Int]->Bool -}

fin :: Int -> [Int] -> Bool

fin x (a:b)
    |x == a = True
    |x /= a = fin x b
    
