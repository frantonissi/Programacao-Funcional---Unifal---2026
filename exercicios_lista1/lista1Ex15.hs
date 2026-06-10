fibonacci :: Int -> [Int]
fibonacci n = fibonacciM n 0 1

fibonacciM :: Int -> Int -> Int -> [Int]
fibonacciM 0 _ _ = []
fibonacciM n x y = x : fibonacciM (n - 1) y (x + y) -- O x nós guardamos na lista, agora o y tem que ser o novo x, e o novo y, tem que ser a soma do novo x com o que vem antes dele

buscaFib :: Int -> Int -> Int -> Int -> Int
buscaFib x pos a b
    | x == a = pos
    | a > x = -1
    | otherwise = buscaFib x (pos + 1) b (a + b)


antiFib :: Int -> Int
antiFib x = buscaFib x 0 0 1
    
