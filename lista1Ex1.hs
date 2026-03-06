{- HLINT ignore "Use guards" -}
f1 :: Float -> Float
f1 x = if x >= 0 then (x+4)/(x+2) else 2/x

f2:: Int -> Int -> Int
f2 x y = if x >= y then x + y else x - y

f3 :: Int -> Int -> Int -> Int
f3 x y z = if (x + y) > z then x + y + z else if (x + y) < z then x - y - z else 0

main :: IO()

main = do
    print (f1 2)
    print (f2 5 10)
    print (f3 11 20 30)