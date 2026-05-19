import Data.Char

conta:: [Char] -> Char -> Int
conta [] x = 0
conta (a:b) x
    |x == a = conta b x + 1
    |otherwise = conta b x