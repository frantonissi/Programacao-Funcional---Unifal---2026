soma::Int -> Int -> Int
soma x y = x + y


mult::Int -> Int -> Int
mult _ 0 = 0
mult m n
    |n /= 0 = soma m  (mult m (n-1))
    --aqui usa recursividade, ou seja, primeiro soma 5 com n sendo 3, ai depois soma 5 com n sendo 2
    --até chegar em somar 5 com n sendo 0, e como m nao importa, a ultima soma seria 5 + mult(_ 0) que definimos
    --que retorna 0, e depois vai voltando somando com os outros 5

main :: IO()
main = do
    print (mult 5 3)




