import Data.Char

converte :: Char -> (Char, Char, Int)
converte a = (a, chr(ord a - 32), ord a)