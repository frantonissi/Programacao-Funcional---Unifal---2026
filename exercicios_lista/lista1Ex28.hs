-- Função principal: passa por cada elemento da lista
proliferaInt :: [Int] -> [Int]
proliferaInt [] = []
proliferaInt (a:b) = repeteElemento a a ++ proliferaInt b

-- Função auxiliar: apenas gera uma lista com o "valor" repetido "n" vezes
repeteElemento :: Int -> Int -> [Int]
repeteElemento 0 _ = []
repeteElemento n valor = valor : repeteElemento (n - 1) valor

