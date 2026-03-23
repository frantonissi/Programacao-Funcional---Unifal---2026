fat::Int -> Int
fat 0 = 1
fat x = x * fat(x - 1)

combinacao::Int -> Int -> Int
combinacao m n
    |m < n = 0
    |otherwise = fat m `div` (fat n * fat(m - n))

main::IO()  

main = do
    --print(fat 5)
    print (combinacao 5 2) -- 10