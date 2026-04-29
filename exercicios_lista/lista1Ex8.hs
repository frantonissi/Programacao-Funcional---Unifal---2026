
mdc :: Int -> Int -> Int

mdc m n
    |m `mod` n == 0 = n
    |otherwise = mdc n (m `mod` n)


