
allDifferent::Int->Int->Int->Bool
allDifferent m n p
    | m /= n && n /= p && m /= p = True --Precisa comparar o m e o p
    |otherwise = False


