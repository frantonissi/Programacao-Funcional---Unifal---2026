
allDifferent::Int->Int->Int->Bool
allDifferent m n p
    | m /= n && n /= p && m /= p = True --Precisa comparar o m e o p
    |otherwise = False


    --O errado é que ele podia cometer o erro de ver que m era diferente de n e p era diferente de n, mas nao comparar
    -- o m com o p, o que poderia resultar num falso e falso o que retorna True mas ainda sim o m ser igual ao p
    

