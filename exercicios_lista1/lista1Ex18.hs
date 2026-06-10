import Data.Char

charToNum::Char->Int
charToNum x 
    |ord x < 48 && ord x > 58 = -1
    |otherwise =  ord x - 48

    