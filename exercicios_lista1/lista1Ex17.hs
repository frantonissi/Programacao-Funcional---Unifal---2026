import Data.Char

converteMinusculo :: Char -> Char
converteMinusculo letra
    |letra >= 'A' && letra <= 'Z' = letra
    |otherwise = chr (ord letra - 32)
    
    