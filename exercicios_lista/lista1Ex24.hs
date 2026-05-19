import Data.Char

converte :: [Int] -> [Char]
converte [] = []
converte (a:b) = chr(a + 64) : converte b 

