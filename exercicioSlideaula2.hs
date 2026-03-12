type Dia = Int
type Venda = Int

f :: Dia -> Venda --recebe dia e retorna venda
f 1 = 5
f 2 = 3
f 3 = 15
f 4 = 14
f 5 = 8
f 6 = 0
f 7 = 3
f x = -1

--Implemente uma funcao que retorne o dia em que houve a
--maior venda

maiorVenda :: Int -> Int

maiorVenda n
    |n == 0 = f 1


main :: IO()

main = do
    print(maiorVenda 1)





