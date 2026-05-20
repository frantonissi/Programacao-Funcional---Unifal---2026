import Data.Char (ord)

-- Função principal
proliferaChar :: [Char] -> [Char]
proliferaChar [] = []
proliferaChar (a:b) = repeteChar (ord a - 64) a ++ proliferaChar b

-- Função auxiliar (o primeiro argumento DEVE ser Int para o contador)
repeteChar :: Int -> Char -> [Char]
repeteChar 0 _ = []
repeteChar n c = c : repeteChar (n - 1) c
